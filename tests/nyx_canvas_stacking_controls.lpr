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
program nyx_canvas_stacking_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Actual owned Win32 windows qualify native/LCL stacking and retained editing.
  No raster, browser host, project store or compositor claim is involved. }
uses
  Interfaces, Forms, Controls, StdCtrls, Classes, SysUtils, Windows, LMessages,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.canvas.geometry,
  nyx.canvas.types, nyx.canvas.scene, nyx.canvas.workspace, nyx.canvas.precision,
  nyx.canvas.lcl;

type
  { The fixture owns this receiver until all windows have retired. Application
    borrows its event slot; native exceptions become ordinary assertion failures. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

  { A fixture-owned spy borrows one stable mounted host and forwards its exact
    WindowProc. It counts actual window-position notifications, not model calls.
    Detach before the view can destroy the host; this spy retains no view. }
  TPositionProbe = class
  private
    FHost: TWinControl;
    FOriginal: TWndMethod;
    procedure ForwardMessage(var AMessage: TLMessage);
  public
    Changes: Integer;
    constructor Create(AHost: TWinControl);
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GFailure: TFailure;

procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Error := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
end;

constructor TPositionProbe.Create(AHost: TWinControl);
begin
  inherited Create;
  FHost := AHost;
  FOriginal := AHost.WindowProc;
  AHost.WindowProc := ForwardMessage;
end;

destructor TPositionProbe.Destroy;
begin
  FHost.WindowProc := FOriginal;
  inherited Destroy;
end;

procedure TPositionProbe.ForwardMessage(var AMessage: TLMessage);
begin

  if AMessage.Msg = LM_WINDOWPOSCHANGED then
  begin
    Inc(Changes);
  end;
  FOriginal(AMessage);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition or (GFailure.Error <> '') then
  begin
    raise Exception.Create(AMessage + ' / ' + GFailure.Error);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize;
end;

function ItemHost(AInput, ASurface: TWinControl): TWinControl;
begin
  Result := AInput;
  while (Result <> nil) and (Result.Parent <> ASurface) do
  begin
    Result := Result.Parent;
  end;
  Check(Result <> nil, 'input belongs to one direct canvas host');
end;

procedure CheckAbove(AHigher, ALower: TWinControl);
var
  LIndex: Integer;
  LHigh: Integer;
  LLow: Integer;
  LHandle: HWND;
begin
  LHigh := -1;
  LLow := -1;
  for LIndex := 0 to AHigher.Parent.ControlCount - 1 do
  begin

    if AHigher.Parent.Controls[LIndex] = AHigher then
    begin
      LHigh := LIndex;
    end;

    if AHigher.Parent.Controls[LIndex] = ALower then
    begin
      LLow := LIndex;
    end;
  end;
  Check((AHigher.Parent = ALower.Parent) and (LHigh > LLow) and (LLow >= 0),
    'LCL child order matches the accepted scene');
  LHandle := GetWindow(AHigher.Handle, GW_HWNDNEXT);
  while (LHandle <> 0) and (LHandle <> ALower.Handle) do
  begin
    LHandle := GetWindow(LHandle, GW_HWNDNEXT);
  end;
  Check(LHandle = ALower.Handle, 'actual native sibling order matches LCL');
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
  LInput: TMemo;
  LButton: TWinControl;
  LMemoHost: TWinControl;
  LButtonHost: TWinControl;
  LTop: TWinControl;
  LLeft: TWinControl;
  LMemoProbe: TPositionProbe;
  LButtonProbe: TPositionProbe;
  LTopProbe: TPositionProbe;
  LLeftProbe: TPositionProbe;
  LLayer: TNyxCanvasLayerRef;
  LNotes: TNyxCanvasItemRef;
  LAction: TNyxCanvasItemRef;
  LBefore: TNyxText;
  LIndex: Integer;
begin
  LHost := nil;
  LDocument := nil;
  LMemoProbe := nil;
  LButtonProbe := nil;
  LTopProbe := nil;
  LLeftProbe := nil;
  try
    LHost := TForm.CreateNew(nil);
    LHost.ClientWidth := 800;

    if ParamStr(1) = '--compact' then
    begin
      LHost.ClientWidth := 390;
    end;
    LHost.ClientHeight := 600;
    LHost.Show;
    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxPage('home'));
    LBefore := TNyxCodec.Encode(LDocument);
    LLayer := NyxCanvasLayer('Content');
    LNotes := NyxCanvasItem('Notes');
    LAction := NyxCanvasItem('Action');
    LScene := NewNyxCanvasScene;
    LScene.AddLayer(LLayer);
    LScene.AddControl(LNotes, NyxCanvasPlacement(LLayer,
      NyxCanvasBounds(-170, -80, 240, 140)),
      NewNyxMemo('notes').WithText('Notes'));
    LScene.AddControl(LAction, NyxCanvasPlacement(LLayer,
      NyxCanvasBounds(-50, 20, 180, 80)), NewNyxButton('send').WithText('Send'));
    LWorkspace := NewNyxCanvasWorkspace('stacking', LScene);
    LOptions := NyxCanvasPrecisionOptions.WithRulers(NyxCanvasRulers.Units(1, 'px'));
    NyxCanvasWorkspacePrecision(LWorkspace).WithPrecision(LOptions);
    LView := MountNyxLCLCanvasWorkspace(LWorkspace, LDocument, LHost);
    Pump;
    LPrecision := LView as INyxCanvasPrecisionView;
    LRulers := LView as INyxLCLCanvasRulerPresentation;
    LTop := LRulers.RulerFor(ncaxX);
    LLeft := LRulers.RulerFor(ncaxY);
    LInput := TMemo(LView.InputFor(LNotes, NyxControl('notes')));
    LButton := TWinControl(LView.RendererFor(LAction).ControlFor('send'));
    LMemoHost := ItemHost(LInput, LView.Surface);
    LButtonHost := ItemHost(LButton, LView.Surface);
    CheckAbove(LButtonHost, LMemoHost);
    CheckAbove(LTop, LButtonHost);
    CheckAbove(LLeft, LTop);
    LInput.SetFocus;
    LInput.Text := 'An unfinished note';
    LInput.SelStart := 3;
    LInput.SelLength := 4;
    LMemoProbe := TPositionProbe.Create(LMemoHost);
    LButtonProbe := TPositionProbe.Create(LButtonHost);
    LTopProbe := TPositionProbe.Create(LTop);
    LLeftProbe := TPositionProbe.Create(LLeft);
    { Positive control proves that the installed native hook observes restacking. }
    LMemoHost.BringToFront;
    Check(LMemoProbe.Changes > 0, 'spy sees a real caller-initiated native reorder');
    LView.Refresh;
    CheckAbove(LButtonHost, LMemoHost);
    CheckAbove(LLeft, LButtonHost);
    LMemoProbe.Changes := 0;
    LButtonProbe.Changes := 0;
    LTopProbe.Changes := 0;
    LLeftProbe.Changes := 0;
    for LIndex := 1 to 4 do
    begin
      LView.Refresh;
    end;
    Check((LMemoProbe.Changes = 0) and (LButtonProbe.Changes = 0) and
      (LTopProbe.Changes = 0) and (LLeftProbe.Changes = 0),
      'stable presentations cause no native sibling-position notifications');
    Check(Windows.SetWindowPos(LMemoHost.Handle, HWND_TOP, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE),
      'fixture changes only native stacking, leaving the LCL list untouched');
    LView.Refresh;
    CheckAbove(LButtonHost, LMemoHost);
    CheckAbove(LTop, LButtonHost);
    CheckAbove(LLeft, LTop);
    LScene.OrderItem(LNotes, 1);
    LView.Refresh;
    CheckAbove(LMemoHost, LButtonHost);
    CheckAbove(LTop, LMemoHost);
    LPrecision.SetPrecision(LOptions.WithRulers(NyxCanvasRulers.Axes([])));
    LView.Refresh;
    CheckAbove(LMemoHost, LButtonHost);
    LScene.OrderItem(LAction, 1);
    LView.Refresh;
    CheckAbove(LButtonHost, LMemoHost);
    LScene.Place(LAction, NyxCanvasPlacement(LLayer,
      NyxCanvasBounds(-50, 20, 180, 80)).Visible(False));
    LView.Refresh;
    Check(not LButtonHost.Visible, 'hidden item stays hidden during reconciliation');
    LScene.Place(LAction, NyxCanvasPlacement(LLayer,
      NyxCanvasBounds(-50, 20, 180, 80)));
    LView.Refresh;
    CheckAbove(LButtonHost, LMemoHost);
    LPrecision.SetPrecision(LOptions);
    CheckAbove(LLeft, LTop);
    CheckAbove(LTop, LButtonHost);
    LPrecision.SetPrecision(LOptions.WithRulers(NyxCanvasRulers.Axes([ncaxX])));
    CheckAbove(LTop, LButtonHost);
    LView.Refresh;
    CheckAbove(LTop, LButtonHost);
    LPrecision.SetPrecision(LOptions);
    CheckAbove(LLeft, LTop);
    Check((LView.InputFor(LNotes, NyxControl('notes')) = LInput) and
      (LView.RendererFor(LAction).ControlFor('send') = LButton),
      'scene reorder and ruler changes retain exact mounted controls');
    Check((LInput.Text = 'An unfinished note') and (LInput.SelStart = 3) and
      (LInput.SelLength = 4) and LInput.Focused,
      'native editing text, caret, selection and focus survive stacking');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'caller document stays exact');
  finally
    LLeftProbe.Free;
    LTopProbe.Free;
    LButtonProbe.Free;
    LMemoProbe.Free;

    if LView <> nil then
    begin
      LView.Disconnect;
    end;
    LRulers := nil;
    LPrecision := nil;
    LView := nil;
    LWorkspace := nil;
    LScene := nil;
    LDocument.Free;
    LHost.Free;
  end;
end;

begin
  Application.Initialize;
  GFailure := TFailure.Create;
  Application.OnException := GFailure.Failed;
  try
    Run;
    WriteLn('PASS ', GChecks, ' native canvas stacking checks');
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
