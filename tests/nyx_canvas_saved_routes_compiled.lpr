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
program nyx_canvas_saved_routes_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.model, nyx.codec, nyx.generated.view,
  nyx.canvas.types, nyx.canvas.declarations, nyx.canvas.route.types,
  nyx.canvas.workspace, nyx.flow.route.model
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled canvas connections: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Execute the exact emitted constructor, compare its whole model and mount saved
  masks into independent portable item trees. This proves construction and mask
  consumption, separately from physical controls, interpreted routes or UI input. }
procedure Verify(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LWorkspace: INyxCanvasWorkspace;
  LOther: INyxCanvasWorkspace;
  LDefinition: INyxCanvasDefinition;
  LChanges: TNyxCanvasRouteChanges;
  LControl: TNyxNode;
  LScene: Integer;
  LIndex: Integer;
begin
  LDocument := BuildNyxDocument;
  try
    Check(TNyxCodec.Encode(LDocument) = AExpected,
      'the actual compiled companion reconstructs every admitted document byte');
    for LScene := 0 to LDocument.Canvases.Count - 1 do
    begin
      LDefinition := LDocument.Canvases.Definition(LDocument.Canvases.Reference(LScene));
      LChanges := NyxCanvasConnectionChanges(LDefinition);
      LWorkspace := NewNyxCanvasWorkspace('compiled-first', LDocument,
        LDocument.Canvases.Reference(LScene));
      LOther := NewNyxCanvasWorkspace('compiled-second', LDocument,
        LDocument.Canvases.Reference(LScene));
      for LIndex := 0 to High(LChanges) do
      begin
        LControl := LWorkspace.Scene.Control(LChanges[LIndex].Item).Node.Find(LChanges[LIndex].Control.ID);
        Check(NyxControlFlowRoutes(LControl).ToData.ToJSON = LChanges[LIndex].Routes.ToData.ToJSON,
          'saved masks configure the exact independently owned item control');
        Check(LControl <> LOther.Scene.Control(LChanges[LIndex].Item).Node.Find(LChanges[LIndex].Control.ID),
          'two constructed workspaces never share the embedded implementation');
      end;
    end;
    Check(TNyxCodec.Encode(LDocument) = AExpected,
      'runtime item construction preserves every accepted source/default byte');
  finally
    LOther := nil;
    LWorkspace := nil;
    LDocument.Free;
  end;
end;

procedure Execute(const AExpected: TNyxText);
begin
  try
    Verify(AExpected);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-compiled-canvas-routes', 'passed');
    document.body.setAttribute('data-compiled-canvas-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' compiled canvas connection checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-compiled-canvas-routes', 'failed');
      document.body.setAttribute('data-compiled-canvas-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;

function Loaded(AEvent: TJSProgressEvent): Boolean;
var
  LExpected: TNyxText;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-compiled-canvas-routes', 'failed');
    document.body.setAttribute('data-compiled-canvas-error', 'Exact expected design is unavailable');
    GRequest := nil;
    Exit;
  end;
  LExpected := GRequest.responseText;
  GRequest := nil;
  Execute(LExpected);
end;

begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'design.nyx', True);
  GRequest.onload := @Loaded;
  GRequest.send;
end.
{$else}
var
  LFile: TFileStream;
  LExpected: TNyxText;

begin

  if ParamCount <> 1 then
  begin
    raise Exception.Create('Expected design.nyx filename is required');
  end;
  LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 16777216) then
    begin
      raise Exception.Create('Expected design is outside the qualification byte budget');
    end;
    SetLength(LExpected, LFile.Size);
    LFile.ReadBuffer(LExpected[1], Length(LExpected));
  finally
    LFile.Free;
  end;
  Execute(LExpected);
end.
{$endif}
