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
program nyx_canvas_precision_controls;
{$mode delphi}{$H+}{$codepage utf8}
uses
  Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, SysUtils,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.canvas.precision, nyx.canvas.lcl;

type
  { Borrow installed native input methods for this owned fixture. The adapter
    retains every returned control; this alias changes no lifetime or class. }
  TControlAccess = class(TWinControl);

  { Fixture-owned receiver retains the first native host exception so a paint
    or queued-input failure cannot open a blocking dialog. It outlives every
    connected view and native window, then releases its borrowed event slot. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

var
  GChecks: Integer;
  GFailure: TFailure;

procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Error := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
end;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if (GFailure <> nil) and (GFailure.Error <> '') then
  begin
    raise Exception.Create(GFailure.Error);
  end;

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
var
  LIndex: Integer;
begin
  for LIndex := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Sleep(10);
  end;
  Check((GFailure = nil) or (GFailure.Error = ''), 'native host turns remain free of errors');
end;

{ Buffered native control rendering, not a foreground/display capture. The
  optional fixture-owned PNG is useful for checking ruler/guide visual layout. }
procedure Capture(AHost: TForm);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamStr(2) = '' then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AHost.ClientWidth, AHost.ClientHeight);
    AHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(2), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure Run;
var
  LHost: TForm;
  LDocument: TNyxDocument;
  LScene: INyxCanvasScene;
  LWorkspace: INyxCanvasWorkspace;
  LView: INyxLCLCanvasWorkspaceView;
  LPrecision: INyxCanvasPrecisionView;
  LRulers: INyxLCLCanvasRulerPresentation;
  LOptions: TNyxCanvasPrecisionOptions;
  LMemo: INyxMemo;
  LInput: TMemo;
  LTop: TWinControl;
  LLeft: TWinControl;
  LBefore: TNyxText;
  LDesign: TNyxText;
  LKey: Word;
  LCount: Integer;
  LSelection: TNyxCanvasGuideRef;
  LCamera: TNyxCanvasCamera;
begin
  LHost := nil;
  LDocument := nil;
  try
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(20, 20, 1280, 900);
    LHost.Caption := 'Precision workspace';

    if ParamStr(1) = '--compact' then
    begin
      LHost.Width := 390;
    end;
    LHost.Show;
    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxPage('home'));
    LDesign := TNyxCodec.Encode(LDocument);
    LScene := NewNyxCanvasScene;
    LScene.AddLayer(NyxCanvasLayer('Content'));
    LMemo := NewNyxMemo('note');
    LMemo.Configure.Text('Notes').Value('Retained editor').Done;
    LScene.AddControl(NyxCanvasItem('Notes'), NyxCanvasPlacement(NyxCanvasLayer('Content'),
      NyxCanvasBounds(-130, -60, 260, 120)), LMemo);
    LWorkspace := NewNyxCanvasWorkspace('precision', LScene);
    LOptions := NyxCanvasPrecisionOptions.WithRulers(NyxCanvasRulers.Units(1, 'px'))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, 40))
      .WithGuide(NyxCanvasGuide(NyxCanvasGuideRef('Top margin'), ncaxY, 160).Locked(True))
      .Snapping(ncgspVisible, 8);
    NyxCanvasWorkspacePrecision(LWorkspace).WithPrecision(LOptions);
    LView := MountNyxLCLCanvasWorkspace(LWorkspace, LDocument, LHost);
    Pump;
    Check(Supports(LView, INyxCanvasPrecisionView, LPrecision), 'real canvas exposes typed runtime precision');
    Check(Supports(LView, INyxLCLCanvasRulerPresentation, LRulers), 'real canvas exposes borrowed ruler faces');
    LTop := LRulers.RulerFor(ncaxX);
    LLeft := LRulers.RulerFor(ncaxY);
    Check((LTop <> nil) and (LLeft <> nil), 'both actual ruler faces are mounted');
    Check((LTop.Height = 24) and (LLeft.Width = 24), 'native rulers consume logical dimensions');
    Check(LPrecision.Precision.ToData.ToJSON = LOptions.ToData.ToJSON, 'mount snapshots exact setup values');
    LInput := TMemo(LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')));
    LInput.Text := 'Unfinished notes';
    LInput.SelStart := 4;
    LCamera := LView.Camera;
    LView.ZoomAt(2, NyxCanvasPoint(LCamera.ViewportWidth / 2, LCamera.ViewportHeight / 2));
    Pump;
    LView.SetCamera(LCamera);
    Pump;
    Check((LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')) = LInput) and
      (LInput.Text = 'Unfinished notes') and (LInput.SelStart = 4),
      'ruler camera updates retain the same native editor, text and caret');
    LBefore := LPrecision.Precision.ToData.ToJSON;
    TControlAccess(LTop).OnMouseDown(LTop, mbLeft, [], 100, 10);
    TControlAccess(LTop).OnMouseMove(LTop, [ssLeft], 100, Round(LCamera.ViewportHeight / 2 + 70));
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'captured guide preview leaves accepted options exact');
    TControlAccess(LTop).OnMouseDown(LTop, mbLeft, [], 150, 10);
    TControlAccess(LTop).OnMouseUp(LTop, mbRight, [], 150, 10);
    Check(LPrecision.Precision.ToData.ToJSON = LBefore,
      'additional presses and unrelated release cannot commit the captured guide');
    LKey := VK_ESCAPE;
    TControlAccess(LTop).OnKeyDown(LTop, LKey, []);
    Check((LKey = 0) and (LPrecision.Precision.ToData.ToJSON = LBefore),
      'installed Escape cancels the preview without accepting guide data');
    TControlAccess(LTop).OnMouseDown(LTop, mbLeft, [], 100, 10);
    TControlAccess(LTop).MouseCapture := False;
    Check((LPrecision.Precision.ToData.ToJSON = LBefore) and
      not LPrecision.SelectedGuide.Defined, 'native capture loss cancels and restores prior guide selection');
    LCount := LPrecision.Precision.Count;
    TControlAccess(LTop).OnMouseDown(LTop, mbLeft, [], 100, 10);
    TControlAccess(LTop).OnMouseMove(LTop, [ssLeft], 100, Round(LCamera.ViewportHeight / 2 + 70));
    TControlAccess(LTop).OnMouseUp(LTop, mbLeft, [], 100, Round(LCamera.ViewportHeight / 2 + 70));
    Check(LPrecision.Precision.Count = LCount + 1, 'one actual ruler release accepts exactly one new guide');
    LSelection := LPrecision.SelectedGuide;
    Check(LSelection.Defined and (LPrecision.Precision.Named(LSelection).Axis = ncaxY),
      'top ruler creates a named horizontal guide');
    Check(Abs(LPrecision.Precision.Named(LSelection).Position - 70) <= 0.5,
      'captured native coordinates project to world guide position');
    LKey := VK_DOWN;
    TControlAccess(LView.Surface).OnKeyDown(LView.Surface, LKey, [ssShift]);
    Check((LKey = 0) and (Abs(LPrecision.Precision.Named(LSelection).Position - 80) <= 0.5),
      'installed Shift-arrow nudges selected guide by ten logical pixels');
    LKey := VK_DELETE;
    TControlAccess(LView.Surface).OnKeyDown(LView.Surface, LKey, []);
    Check((LKey = 0) and (LPrecision.Precision.Count = LCount) and
      not LPrecision.SelectedGuide.Defined, 'installed Delete removes only the selected movable guide');
    LPrecision.SelectGuide(NyxCanvasGuideRef('Top margin'));
    LBefore := LPrecision.Precision.ToData.ToJSON;
    LKey := VK_DELETE;
    TControlAccess(LView.Surface).OnKeyDown(LView.Surface, LKey, []);
    Check(LPrecision.Precision.ToData.ToJSON = LBefore, 'locked guide survives ordinary Delete input');
    Capture(LHost);
    LPrecision.SetPrecision(LOptions.WithRulers(NyxCanvasRulers.Axes([])));
    Check((LRulers.RulerFor(ncaxX) = nil) and (LRulers.RulerFor(ncaxY) = nil),
      'caller can hide rulers without losing guides');
    Check(LView.InputFor(NyxCanvasItem('Notes'), NyxControl('note')) = LInput,
      'precision reconfiguration preserves the mounted input');
    Check(TNyxCodec.Encode(LDocument) = LDesign, 'runtime precision never edits the authored document');
    Check(NyxCanvasWorkspacePrecision(LWorkspace).Precision.ToData.ToJSON = LOptions.ToData.ToJSON,
      'runtime guide edits never rewrite shared setup options');
    LPrecision := nil;
    LRulers := nil;
    LView.Disconnect;
    Check(not LView.Connected, 'view disconnect revokes native input');
    LView := nil;
    LMemo := nil;
    LWorkspace := nil;
    LScene := nil;
  finally

    if LView <> nil then
    begin
      LView.Disconnect;
    end;
    LPrecision := nil;
    LRulers := nil;
    LView := nil;
    LMemo := nil;
    LWorkspace := nil;
    LScene := nil;
    LDocument.Free;
    LHost.Free;
    Pump;
  end;
end;

begin
  Application.Initialize;
  GFailure := TFailure.Create;
  Application.OnException := GFailure.Failed;
  try
    Run;
    WriteLn('PASS ', GChecks, ' native precision control checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  Application.OnException := nil;
  GFailure.Free;
end.
