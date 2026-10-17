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
program nyx_drawing_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.drawing, nyx.drawing.view, nyx.canvas.geometry, nyx.design.tokens,
  nyx.colors, nyx.theme, nyx.codec, nyx.codegen, nyx.source, nyx.schema,
  nyx.test.drawing.fixture
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Drawing: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Run;
var
  LBuilder: INyxDrawingBuilder;
  LPlan: INyxDrawingPlan;
  LCopy: INyxDrawingPlan;
  LPath: TNyxDrawingPath;
  LChanged: TNyxDrawingPath;
  LPoints: TNyxDrawingPoints;
  LMapping: TNyxDrawingMapping;
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LDecoded: TNyxDocument;
  LSource: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LBefore: TNyxText;
  LUnicode: TNyxText;
  LFailed: Boolean;
  LDrawing: INyxDrawing;
  LTheme: TNyxTheme;
  LBudget: INyxDrawingBuilder;
  LIndex: Integer;
begin
  LUnicode := 'Dedicated coverage 🐾 𝄞 漢字';
  LPath := NyxDrawingPath(NyxCanvasPoint(0, 0)).LineTo(NyxCanvasPoint(10, 0));
  LChanged := LPath.CurveTo(NyxCanvasPoint(10, 10), NyxCanvasPoint(20, 10),
    NyxCanvasPoint(20, 0)).Close;
  Check(LPath.ToData.Count = 2, 'immutable path copy');
  LPoints := NyxDrawingPathPoints(LChanged);
  Check(Length(LPoints) = 35, 'bounded shared cubic samples and closure');
  Check((LPoints[High(LPoints)].X = 0) and (LPoints[High(LPoints)].Y = 0), 'closed path origin');
  Check(Abs(LPoints[17].Y - 7.5) < 0.000001, 'shared cubic midpoint');
  LFailed := False;
  try
    LChanged.LineTo(NyxCanvasPoint(1, 1));
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed, 'closed path continuation refuses');
  LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100));
  LBuilder.Path(LChanged, NyxDrawingStyle.Fill(NyxDrawingPaint(NyxRGB(0, 0, 0)))
    .Stroke(NyxThemePaint(ntcAccent), 2));
  LPlan := LBuilder.Build;
  LBuilder.Text(LUnicode, NyxCanvasPoint(10, 20), NyxThemePaint(ntcText));
  Check(LPlan.Count = 1, 'published plan retains independent commands');
  LCopy := LBuilder.Build;
  Check(LCopy.Command(1).Field('text').AsText = LUnicode, 'exact supplementary text');
  Check(NyxDrawingPlanFromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON, 'strict normalized wire');
  LFailed := False;
  try
    LBuilder.Text('NUL' + TNyxText(#0), NyxCanvasPoint(0, 0), NyxThemePaint(ntcText));
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed and (LBuilder.Build.Count = 2), 'failed text preserves complete builder');
  LFailed := False;
  try
    LBuilder.Path(NyxDrawingPath(NyxCanvasPoint(-1, 0)), NyxDrawingStyle);
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed and (LBuilder.Build.Count = 2), 'out of bounds command refuses atomically');
  LBefore := LBuilder.Build.ToData.ToJSON;
  LFailed := False;
  try
    LBuilder.Path(Default(TNyxDrawingPath), NyxDrawingStyle);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.ToData.ToJSON = LBefore),
    'an absent typed path refuses without changing already admitted commands');
  LFailed := False;
  try
    LBuilder.Path(NyxDrawingPath(NyxCanvasPoint(10, 10)).CurveTo(
      NyxCanvasPoint(-1, 10), NyxCanvasPoint(20, 20), NyxCanvasPoint(30, 30)), NyxDrawingStyle);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.ToData.ToJSON = LBefore),
    'every cubic control point remains constrained even when both endpoints are inside');
  LFailed := False;
  try
    LBuilder.Path(TNyxDrawingPath.FromData(NyxArray([
      NyxArray([NyxData(0), NyxArray([NyxData(0), NyxData(0)])]),
      NyxArray([NyxData(77)])])), NyxDrawingStyle);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.ToData.ToJSON = LBefore),
    'unknown persisted path segments still refuse before typed builder admission');
  LFailed := False;
  try
    LBuilder.Path(LPath, TNyxDrawingStyle.FromData(NyxObject([
      NyxField('fill', NyxNull), NyxField('stroke', NyxNull), NyxField('width', NyxData(1))])));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.ToData.ToJSON = LBefore),
    'persisted absent-stroke width still refuses without changing the complete builder');
  LFailed := False;
  try
    NyxDrawingProvider('   ');
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed, 'provider identity requires intent');
  LBudget := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100));
  LChanged := NyxDrawingPath(NyxCanvasPoint(0, 0));
  for LIndex := 1 to 100 do
  begin
    LChanged := LChanged.CurveTo(NyxCanvasPoint(10, 10), NyxCanvasPoint(20, 10),
      NyxCanvasPoint(0, 0));
  end;
  for LIndex := 1 to 20 do
  begin
    LBudget.Path(LChanged, NyxDrawingStyle);
  end;
  LFailed := False;
  try
    LBudget.Path(LChanged, NyxDrawingStyle);
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed and (LBudget.Build.Count = 20), 'sample budget refuses before appending');
  LChanged := NyxDrawingPath(NyxCanvasPoint(0, 0));
  for LIndex := 1 to MaximumNyxDrawingPathSteps - 1 do
  begin
    LChanged := LChanged.CurveTo(NyxCanvasPoint(10, 10), NyxCanvasPoint(20, 10),
      NyxCanvasPoint(0, 0));
  end;
  LFailed := False;
  try
    LChanged.LineTo(NyxCanvasPoint(0, 0));
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed and (LChanged.ToData.Count = MaximumNyxDrawingPathSteps),
    'maximum immutable path capacity refuses without reparsing predecessors');
  LFailed := False;
  try
    NyxDrawingPlanFromData(NyxObject([NyxField('version', NyxData(2)),
      NyxField('box', NyxArray([NyxData(0), NyxData(0), NyxData(1), NyxData(1)])),
      NyxField('commands', NyxArray([]))]));
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed, 'unknown version refuses');
  LMapping := NyxDrawingMapping(LCopy, 200, 100);
  Check((LMapping.Scale = 1) and (LMapping.Left = 50) and (LMapping.Top = 0), 'contain mapping centers');
  LFailed := False;
  try
    NyxDrawingMapping(LCopy, 32769, 100);
  except
    on E: Exception do LFailed := True;
  end;
  Check(LFailed, 'native-safe shared allocation limit');
  LTheme := TNyxTheme.Create;
  try
    Check(ResolveNyxDrawingPaint(NyxThemePaint(ntcAccent), LTheme).ToText =
      LTheme.Accent, 'semantic paint resolves theme');
    Check(NyxDrawingPalette(LTheme).ColorValue(ntcSurface).ToText = LTheme.Surface,
      'provider frame palette is typed');
  finally
    LTheme.Free;
  end;
  LDrawing := NewNyxDrawing('typed-graphic').WithPlan(LCopy)
    .WithProvider(NyxDrawingProvider('custom'));
  Check(LDrawing.Plan.ToData.ToJSON = LCopy.ToData.ToJSON, 'specialized managed plan');
  Check(LDrawing.Provider.Name = 'custom', 'distinct typed provider reference');
  Check(NewNyxDrawing('starter-graphic').Plan.Count > 0, 'toolbox starter has useful themed geometry');
  LDocument := DrawingFixture;
  LParsed := nil;
  LDecoded := nil;
  LWorkspace := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'saved scene resources and drawing round trip');
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('LConnectionGraphicDrawing: INyxDrawing', LSource) > 0, 'purpose and specialized type');
    Check((Pos('SetNyxDrawingPlan(', LSource) > 0) and (Pos('.CurveTo(', LSource) > 0) and
      (Pos('NyxThemePaint(ntcAccent)', LSource) > 0), 'crafted typed drawing source');
    Check(Pos('NyxExtension(' + TNyxText(#39) + 'nyx.drawing', LSource) = 0, 'known declarations avoid raw extension authoring');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'complete literal source reconstruction');
    Check(TNyxCodegen.Generate(LParsed) = LSource, 'deterministic no-op generation');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LBefore);
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-drawing', 'passed');
    document.body.setAttribute('data-drawing-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable drawing/source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-drawing', 'failed');
      document.body.setAttribute('data-drawing-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
