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
program nyx_canvas_continuation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data,
  nyx.codec, nyx.codegen, nyx.source, nyx.composition, nyx.root.types,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations,
  nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;
  {$ifdef PAS2JS}GRequest: TJSXMLHttpRequest;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Canvas continuation: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure ExportStage(AStage: Integer; const ASource, ADesign: TNyxText);
var
  LDirectory: TNyxText;
  LStream: TFileStream;

  procedure WriteBytes(const AName, AValue: TNyxText);
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(AValue) > 0 then
      begin
        LStream.WriteBuffer(AValue[1], Length(AValue));
      end;
    finally
      LStream.Free;
    end;
  end;
begin
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(2)) +
    'stage-' + TNyxText(IntToStr(AStage)) + PathDelim;
  ForceDirectories(LDirectory);
  WriteBytes('nyx.generated.view.pas', ASource);
  WriteBytes('design.nyx', ADesign);
end;
{$endif}

procedure Run(const ASource: TNyxText);
var
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LUntouched: TNyxText;
  LSource: TNyxText;
  LNextSource: TNyxText;
  LOriginalHelpers: TNyxText;
  LPosition: Integer;
  LStage: Integer;
  LDefinition: INyxCanvasDefinition;
  LWorkspace: TNyxSourceWorkspace;
  LRefused: Boolean;
begin
  { This base was constructed by the exact compiled handwritten unit. The
    caller supplies its exact bytes, rather than reconstructing a literal model. }
  LBefore := BuildNyxDocument;
  LAfter := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.AcceptExecuted(LBefore, ASource);
    Check(LWorkspace.Render(LBefore) = ASource, 'actual base retains exact executed source bytes');
    LSource := ASource;
    LPosition := Pos('function SceneSpacing', ASource);
    Check(LPosition > 0, 'compiled fixture exposes its exact handwritten helper boundary');
    LOriginalHelpers := Copy(ASource, LPosition,
      Pos('function BuildNyxDocument: TNyxDocument;' + #10 + 'begin', ASource) - LPosition);
    LUntouched := LBefore.Canvases.Definition(NyxCanvasRef('Second')).ToData.ToJSON;
    for LStage := 1 to 5 do
    begin
      LAfter := LBefore.Clone;
      case LStage of
        1:
          begin
            LAfter.Find('alternate').Configure.NoCanvasScene.Done;
            LAfter.Find('wrapped-scene').Configure.InheritCanvasScene.Done;
          end;
        2:
          begin
            LAfter.Find('alternate').Configure.InheritCanvasScene.Done;
            LDefinition := LAfter.Canvases.Definition(NyxCanvasRef('First'))
              .WithGrid(NyxCanvasGrid(31.5, ncgsLines, ncsmGrid));
            LAfter.Canvases.Define(NyxCanvasRef('First'), LDefinition);
          end;
        3:
          begin
            LAfter.Canvases.Define(NyxCanvasRef('Third'),
              LAfter.Canvases.Definition(NyxCanvasRef('First')).WithScaling(nccsLogicalSize));
            LAfter.Find('alternate').Configure.CanvasScene(NyxCanvasRef('Third')).Done;
            LAfter.Find('canvas-template').Configure.CanvasScene(NyxCanvasRef('Second')).Done;
            LAfter.Find('wrapped-board').Configure.CanvasScene(NyxCanvasRef('Second')).Done;
            LAfter.Canvases.Remove(NyxCanvasRef('First'));
          end;
        4:
          begin
            LAfter.Canvases.Define(NyxCanvasRef('First'),
              LAfter.Canvases.Definition(NyxCanvasRef('Third'))
                .WithCamera(NyxCanvasCamera(400, 300, NyxCanvasPoint(12.5, -7.25))));
            LAfter.Find('alternate').Configure.CanvasScene(NyxCanvasRef('First')).Done;
          end;
        5:
          begin
            LAfter.Find('alternate').Configure.InheritCanvasScene.Done;
            LAfter.Canvases.Remove(NyxCanvasRef('Third'));
          end;
      end;
      LAfter.Validate;
      LNextSource := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
      Check(Pos(LOriginalHelpers, LNextSource) > 0, 'handwritten helper bytes survive stage ' + IntToStr(LStage));
      Check(LAfter.Canvases.Definition(NyxCanvasRef('Second')).ToData.ToJSON = LUntouched,
        'unmodified computed scene remains exact stage ' + IntToStr(LStage));
      Check(Pos('// nyx:continuation ''' + TNyxText(IntToStr(LStage)) + '''', LNextSource) > 0,
        'chronological source stages retain independent local scopes ' + IntToStr(LStage));
      Check(Pos('Canvases.Define(NyxCanvasRef(''Second'')', Copy(LNextSource, Length(ASource), MaxInt)) = 0,
        'unchanged helper scene is never overwritten by literal source');
      {$ifndef PAS2JS}ExportStage(LStage, LNextSource, TNyxCodec.Encode(LAfter));{$endif}
      LBefore.Free;
      LBefore := LAfter;
      LAfter := nil;
      LSource := LNextSource;
    end;
    { A future handwritten helper change must still influence the untouched
      plan. Changed First remains literal because the designer actually edited
      it; Second continues to call the retained helper on every construction. }
    LPosition := Pos('Result := 12.25 * 2;', LSource);
    Check(LPosition > 0, 'future helper edit addresses exact retained source');
    LNextSource := Copy(LSource, 1, LPosition - 1) + 'Result := 13.25 * 2;' +
      Copy(LSource, LPosition + Length('Result := 12.25 * 2;'), MaxInt);
    LAfter := LBefore.Clone;
    LAfter.Canvases.Define(NyxCanvasRef('Second'),
      LAfter.Canvases.Definition(NyxCanvasRef('Second'))
        .WithGrid(NyxCanvasGrid(26.5, ncgsDots, ncsmFree)));
    {$ifndef PAS2JS}ExportStage(6, LNextSource, TNyxCodec.Encode(LAfter));{$endif}
    Check(LAfter.Canvases.Definition(NyxCanvasRef('First')).Grid.Spacing = 31.5,
      'deliberately edited scene remains independent of future helper output');
    FreeAndNil(LAfter);
    LAfter := LBefore.Clone;
    LAfter.Title := 'Only a caption changed';
    LNextSource := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    Check(Pos(LOriginalHelpers, LNextSource) > 0, 'later ordinary source edit retains complete scene continuation');
    FreeAndNil(LAfter);
    LAfter := LBefore.Clone;
    LAfter.Extensions.SetValue(NyxExtension('unexpressed-review'), NyxData(True));
    LRefused := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LWorkspace.Capture.Source = ASource),
      'unexpressed meaning refuses rather than replacing the original authored unit');
    FreeAndNil(LAfter);
    LAfter := LBefore.Clone;
    LDefinition := LAfter.Canvases.Definition(NyxCanvasRef('Second'));
    LAfter.Canvases.Remove(NyxCanvasRef('Second'));
    LAfter.Canvases.Define(NyxCanvasRef('Second'), LDefinition);
    LRefused := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LWorkspace.Capture.Source = ASource),
      'unexpressed scene registry reordering refuses the complete proposal');
  finally
    LWorkspace.Free;
    LAfter.Free;
    LBefore.Free;
  end;
end;

procedure Execute(const ASource: TNyxText);
begin
  try
    Run(ASource);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-continuation', 'passed');
    document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' handwritten canvas continuation checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-continuation', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-canvas-continuation', 'failed');
    document.body.setAttribute('data-canvas-error', 'Exact handwritten fixture source unavailable');
    Exit;
  end;
  Execute(GRequest.responseText);
  GRequest := nil;
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'nyx.generated.view.pas');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LSource, LStream.Size);
    LStream.ReadBuffer(LSource[1], Length(LSource));
  finally
    LStream.Free;
  end;
  Execute(LSource);
  {$endif}
end.
