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
program nyx_canvas_palette_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.canvas.types, nyx.canvas.workspace, nyx.canvas.declarations,
  nyx.canvas.palette
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled canvas palette: ' + AReason);
  end;
  Inc(GChecks);
end;

{ The actual Pascal compiler executes the exact exported constructor. Whole
  document equality qualifies source meaning; portable item construction below
  remains separate from actual browser/LCL widget and input qualification. }
procedure Execute(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LOwner: TNyxControlRef;
  LScene: TNyxCanvasRef;
  LDefinition: INyxCanvasDefinition;
  LWorkspace: INyxCanvasWorkspace;
begin
  LDocument := nil;
  try
    LDocument := BuildNyxDocument;
    Check(TNyxCodec.Encode(LDocument) = AExpected, 'entire compiled document equals its exact accepted design');
    Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
      'compiled Tool Canvas retains its exact source workspace part');
    LScene := LDocument.Find(LOwner.ID).CanvasScene;
    LDefinition := LDocument.Canvases.Definition(LScene);
    Check(LDefinition.ItemCount > 0, 'compiled definition retains authored items');
    LWorkspace := NewNyxCanvasWorkspace('compiled-palette-workspace', LDocument, LScene);
    Check(LWorkspace.Scene.Control(LDefinition.Items[0].Item).Node <> nil,
      'compiled root references materialize independently owned controls');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-palette-compiled', 'passed');
    document.body.setAttribute('data-canvas-palette-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' exact compiled canvas palette constructor checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-palette-compiled', 'failed');
      document.body.setAttribute('data-canvas-palette-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LWorkspace := nil;
  LDocument.Free;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
var
  LExpected: TNyxText;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-canvas-palette-compiled', 'failed');
    document.body.setAttribute('data-canvas-palette-error', 'Exact expected design is unavailable');
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
  LStream: TFileStream;
  LExpected: TNyxText;
begin

  if ParamCount <> 1 then
  begin
    raise Exception.Create('Supply the exact accepted design file');
  end;
  LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 16777216) then
    begin
      raise Exception.Create('Expected design is outside the qualification byte budget');
    end;
    SetLength(LExpected, LStream.Size);
    LStream.ReadBuffer(LExpected[1], Length(LExpected));
  finally
    LStream.Free;
  end;
  Execute(LExpected);
end.
{$endif}
