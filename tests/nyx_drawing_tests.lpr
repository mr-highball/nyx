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
  SysUtils, Math, fpjson, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.drawing, nyx.drawing.view, nyx.canvas.geometry, nyx.design.tokens,
  nyx.colors, nyx.theme, nyx.codec, nyx.codegen, nyx.source, nyx.schema,
  nyx.test.drawing.fixture, nyx.test.drawing.capture
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

{ Exact source reuse is an admission/lifetime contract, not a timing assertion.
  Independent readers own separate normalized plans. Changed metadata does not
  invalidate geometry, but bytes/presence/encoding changes must. Restore the
  caller's formatter even after a failure; no target or application is mounted. }
procedure DrawingSources;
var
  LSource: INyxDrawingSource;
  LOther: INyxDrawingSource;
  LControl: INyxDrawing;
  LPlan: INyxDrawingPlan;
  LFirst: INyxDrawingPlan;
  LSecond: INyxDrawingPlan;
  LAccepted: INyxDrawingPlan;
  LEmpty: INyxDrawingPlan;
  LBuilder: INyxDrawingBuilder;
  LBefore: TNyxText;
  LStrict: Boolean;
  LFailed: Boolean;
  LIndex: Integer;
begin
  LStrict := TJSONString.StrictEscaping;
  try
    TJSONString.StrictEscaping := False;
    LPlan := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 160, 100))
      .Text('Paths / café 𝄞', NyxCanvasPoint(12, 20), NyxThemePaint(ntcText)).Build;
    LControl := NewNyxDrawing('source').WithPlan(LPlan);
    LSource := NewNyxDrawingSource;
    LOther := NewNyxDrawingSource;
    LFirst := LSource.Read(LControl.Node);
    LBefore := LFirst.ToData.ToJSON;
    Check((LSource.Read(LControl.Node) = LFirst) and (LBefore = LPlan.ToData.ToJSON),
      'unchanged complete source retains its exact admitted fallback');
    LSecond := LOther.Read(LControl.Node);
    Check((LSecond <> LFirst) and (LSecond.ToData.ToJSON = LBefore),
      'independent readers do not share mutable cache ownership');
    LControl.WithText('Another caption').WithProvider(NyxDrawingProvider('later'));
    Check(LSource.Read(LControl.Node) = LFirst,
      'caption/provider metadata never substitutes for fallback source identity');
    LControl.WithPlan(NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 200, 120))
      .Text('Changed geometry', NyxCanvasPoint(15, 30), NyxThemePaint(ntcAccent)).Build);
    LSecond := LSource.Read(LControl.Node);
    Check((LSecond <> LFirst) and (LSecond.ViewBox.Width = 200) and
      (LFirst.ToData.ToJSON = LBefore), 'replacement leaves the retained prior fallback exact');
    LAccepted := LSecond;
    LFailed := False;
    try
      LAccepted := LSource.Read(nil);
    except
      on E: ENyxDrawing do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LAccepted = LSecond), 'missing source preserves the previous accepted result');
    LControl.Extensions.SetValue(NyxExtension(NyxDrawingKey), NyxNull);
    LFailed := False;
    try
      LAccepted := LSource.Read(LControl.Node);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LAccepted = LSecond), 'malformed changed source refuses without cached publication');
    LControl.WithPlan(LSecond);
    Check(LSource.Read(LControl.Node) = LSecond, 'a refused key never displaces the previous valid source');
    LControl.Extensions.Remove(NyxExtension(NyxDrawingKey));
    LEmpty := LSource.Read(LControl.Node);
    Check((LEmpty.Count = 0) and (LEmpty.ViewBox.Width = 100) and
      (LSource.Read(LControl.Node) = LEmpty), 'extension removal restores the ordinary reusable empty fallback');
    LControl.WithPlan(LEmpty);
    Check(LSource.Read(LControl.Node) <> LEmpty, 'explicit and absent descriptors have distinct admission keys');
    LControl.WithPlan(LPlan);
    LAccepted := LSource.Read(LControl.Node);
    TJSONString.StrictEscaping := True;
    LSecond := LSource.Read(LControl.Node);
    Check((LSecond <> LAccepted) and (Pos('\/', LSecond.ToData.ToJSON) > 0) and
      (LSource.Read(LControl.Node) = LSecond), 'encoding changes re-admit once with the caller preference');
    TJSONString.StrictEscaping := False;
    LAccepted := LSource.Read(LControl.Node);
    Check(LAccepted.ToData.ToJSON = LBefore, 'restoring encoding retains supplementary Unicode and exact original bytes');
    LControl := nil;
    LSource := nil;
    Check(LAccepted.ToData.ToJSON = LBefore, 'retained geometry outlives its node and reader');

    { Same raw descriptor, different normalized output budget: 128 slash-rich
      text commands fit plainly but exceed 1 MiB with strict escaping. A cached
      read must perform the same exact refusal as a fresh strict decoder. }
    LBuilder := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100));
    for LIndex := 1 to 128 do
    begin
      LBuilder.Text(TNyxText(StringOfChar('/', 4096)), NyxCanvasPoint(0, 0), NyxThemePaint(ntcText));
    end;
    LControl := NewNyxDrawing('budget').WithPlan(LBuilder.Build);
    LSource := NewNyxDrawingSource;
    LAccepted := LSource.Read(LControl.Node);
    Check(NyxUTF8ByteCount(LAccepted.ToData.ToJSON) < MaximumNyxDrawingBytes,
      'plain near-limit source is genuinely valid before its policy changes');
    TJSONString.StrictEscaping := True;
    LFailed := False;
    try
      LSecond := LSource.Read(LControl.Node);
    except
      on E: ENyxDrawing do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'cached source still enforces exact normalized bytes after stricter encoding');
    TJSONString.StrictEscaping := False;
    Check(LSource.Read(LControl.Node) = LAccepted, 'failed strict admission retains the last valid plain source');
  finally
    TJSONString.StrictEscaping := LStrict;
  end;
end;

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
  LCaptured: INyxDrawingPlan;
  LForeign: INyxDrawingPlan;
  LCreator: TNyxTestDrawingPlan;
  LLive: Integer;
  LCommands: INyxDrawingCommands;
  LAdvertised: INyxDrawingCommands;
  LTyped: TNyxDrawingCommand;
  LDerived: TNyxDrawingPath;
  LByteText: TNyxText;
  LNumeric: INyxDrawingPlan;
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
  LCaptured := CaptureNyxDrawingPlan(LPlan);
  Check(LCaptured = LPlan, 'exact admitted built-in plans retain their immutable identity');
  LFailed := False;
  try
    LCaptured := CaptureNyxDrawingPlan(nil);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCaptured = LPlan), 'missing capture refuses without replacing accepted output');
  LBuilder.Text(LUnicode, NyxCanvasPoint(10, 20), NyxThemePaint(ntcText));
  Check(LPlan.Count = 1, 'published plan retains independent commands');
  LCopy := LBuilder.Build;
  Check(LCopy.Command(1).Field('text').AsText = LUnicode, 'exact supplementary text');
  Check(NyxDrawingPlanFromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON, 'strict normalized wire');
  LCommands := NyxDrawingCommands(LCopy);
  Check((LCommands.Count = LCopy.Count) and
    (LCommands.TypedCommand(0).ToData.ToJSON = LCopy.Command(0).ToJSON) and
    (LCommands.TypedCommand(1).ToData.ToJSON = LCopy.Command(1).ToJSON),
    'typed command values preserve the complete legacy wire vocabulary and order');
  LTyped := LCommands.TypedCommand(0);
  Check((LTyped.Kind = ndckPath) and (LTyped.Path.Step(0).Kind = ndskMove) and
    (LTyped.Path.Step(2).Kind = ndskCurve) and (LTyped.Path.Step(2).Point(1).X = 20),
    'typed path commands expose immutable move/curve instructions and control points');
  LTyped := LCommands.TypedCommand(1);
  Check((LTyped.Kind = ndckText) and (LTyped.Text = LUnicode) and
    (LTyped.Position.X = 10) and (LTyped.Paint.Role = ntcText) and (LTyped.Size = 14),
    'typed text retains supplementary scalars, position, semantic paint and font size');
  LFailed := False;
  try
    LDerived := LTyped.Path;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'kind-specific command reads refuse a mismatched value');
  LFailed := False;
  try
    LCommands.TypedCommand(-1);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'typed reads retain bounded display-list indexing');
  LFailed := False;
  try
    Default(TNyxDrawingStep).Point(0);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'default typed steps cannot silently supply an origin');
  LFailed := False;
  try
    Default(TNyxDrawingCommand).ToData;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'absent command serialization refuses instead of becoming a path');
  LFailed := False;
  try
    LCommands.TypedCommand(0).Path.Step(4);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'typed path reads refuse the first step past the end');
  LFailed := False;
  try
    LCommands.TypedCommand(0).Path.Step(3).Point(0);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'close instructions contain no fabricated point');
  LDerived := LPath.LineTo(NyxCanvasPoint(40, 40));
  Check((LPath.StepCount = 2) and (LDerived.StepCount = 3) and
    (LCommands.TypedCommand(0).Path.StepCount = 4),
    'extending a typed path preserves earlier records and published command storage');
  LLive := LiveNyxTestDrawingPlans;
  LCreator := TNyxTestDrawingPlan.Create(LCopy);
  LCreator.ForwardObject := True;
  LForeign := LCreator;
  LCaptured := CaptureNyxDrawingPlan(LForeign);
  {$ifndef PAS2JS}
  Check(LCreator.ObjectQueries > 0, 'native aggregation probe actually forwards the object-instance query');
  {$endif}
  Check((LCreator.WireReads = 1) and (LCreator.GetterReads = 0) and
    (LCaptured.ToData.ToJSON = LCopy.ToData.ToJSON),
    'creator plans capture one complete wire value without traversing mutable getters');
  Check(LCaptured <> LForeign, 'creator class cannot claim the private admitted storage type');
  LCreator.Base := LPlan;
  Check((LCaptured.Count = 2) and (LForeign.GetCount = 1),
    'later creator replacement does not alter the independent captured display list');
  LCreator.FailWire := True;
  LFailed := False;
  try
    LCaptured := CaptureNyxDrawingPlan(LForeign);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LCreator.WireReads = 2) and (LCaptured.Count = 2),
    'valid live getters cannot conceal malformed creator wire or replace accepted output');
  Check(Supports(LForeign, INyxDrawingCommands, LAdvertised),
    'creator fixture genuinely advertises its own typed capability');
  LAdvertised := nil;
  LCreator.FailWire := False;
  LCreator.Base := LCopy;
  LCommands := NyxDrawingCommands(LForeign);
  Check((LCreator.TypedReads = 0) and (LCreator.WireReads = 3) and
    (LCommands.TypedCommand(1).Text = LUnicode),
    'typed reader lookup captures creator wire before considering optional capabilities');
  LCreator := nil;
  LForeign := nil;
  Check((LiveNyxTestDrawingPlans = LLive) and (LCaptured.ToData.ToJSON = LCopy.ToData.ToJSON),
    'captured geometry outlives the creator without retaining its facade');
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
  { A conservative byte bound is allowed to require an exact check, never to
    reject a large valid plan. These two fixtures distinguish that acceptance
    from the actual UTF-8 limit, independently of the bound's arithmetic. }
  LByteText := TNyxText(StringOfChar('x', 4096));
  LBudget := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 100, 100));
  for LIndex := 1 to 64 do
  begin
    LBudget.Text(LByteText, NyxCanvasPoint(0, 0), NyxThemePaint(ntcText));
  end;
  Check((LBudget.Build.Count = 64) and
    (NyxUTF8ByteCount(LBudget.Build.ToData.ToJSON) < MaximumNyxDrawingBytes),
    'exact byte fallback admits large valid text plans without conservative false refusals');
  for LIndex := 65 to 256 do
  begin
    LBudget.Text(LByteText, NyxCanvasPoint(0, 0), NyxThemePaint(ntcText));
  end;
  LFailed := False;
  try
    LBudget.Build;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'typed live construction still refuses a complete oversized wire value');
  LBudget := NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 1, 1));
  for LIndex := 1 to MaximumNyxDrawingCommands do
  begin
    LBudget.Text('', NyxCanvasPoint(0, 0), Default(TNyxDrawingPaint));
  end;
  LBefore := LBudget.Build.ToData.ToJSON;
  Check(LBudget.Build.Count = MaximumNyxDrawingCommands,
    'the full command capacity remains publishable within the exact byte limit');
  LFailed := False;
  try
    LBudget.Text('', NyxCanvasPoint(0, 0), Default(TNyxDrawingPaint));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBudget.Build.ToData.ToJSON = LBefore),
    'overflowing command membership preserves the complete previously admitted builder');
  { Checked builds independently compare the conservative live-byte bound to
    canonical output. Release builds exercise its serialization-free branch.
    Wide/near-zero numbers, literal/theme/transparent paints and escaped text
    qualify the framing assumptions without changing admission or the codec. }
  LNumeric := NewNyxDrawingPlan(NyxCanvasBounds(-1.0e12, -1.0e12, 1.0e12, 1.0e12))
    .Path(NyxDrawingPath(NyxCanvasPoint(-1.0e12, -1.0e12))
      .CurveTo(NyxCanvasPoint(-1.0e-300, -0.00000123456789012345),
        NyxCanvasPoint(-123456789012.34567, -1), NyxCanvasPoint(0, 0)),
      NyxDrawingStyle.Fill(NyxDrawingPaint(TNyxRGBColor.FromText('#abcdef')))
        .Stroke(NyxThemePaint(ntcAccent), 0.12345678901234567))
    .Text('Quote " and slash \ ' + LUnicode, NyxCanvasPoint(-1.0e-300, 0),
      NyxDrawingPaint(TNyxRGBColor.FromText('#123456')), 512, ndtaEnd, True)
    .Text('', NyxCanvasPoint(0, 0), Default(TNyxDrawingPaint), 1)
    .Build;
  Check(NyxDrawingPlanFromData(LNumeric.ToData).ToData.ToJSON = LNumeric.ToData.ToJSON,
    'typed live-byte admission preserves extreme finite coordinates and escaped Unicode');
  LTyped := NyxDrawingCommands(LNumeric).TypedCommand(0);
  Check((LTyped.Path.Step(1).Point(0).X = NyxCanvasPoint(-1.0e-300, 0).X) and
    (LTyped.Style.FillPaint.Color.ToText = '#abcdef') and
    (NyxDrawingCommands(LNumeric).TypedCommand(1).Anchor = ndtaEnd) and
    NyxDrawingCommands(LNumeric).TypedCommand(1).Bold,
    'numeric, literal-paint and font values remain typed through complete capture');
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
    DrawingSources;
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
