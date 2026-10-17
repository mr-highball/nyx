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
program nyx_drawing_buffer_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Actual owned Win32 windows qualify viewport bounds, supplied-DC compatibility,
  callback cardinality, UI ownership and retirement. They save no screenshot,
  raster or project and make no visual/compositor/hardware quality claim. }
uses
  Interfaces, Forms, Classes, SysUtils, Windows, CommCtrl, Types, Controls,
  ExtCtrls, StdCtrls, Graphics, nyx.text, nyx.theme, nyx.colors,
  nyx.drawing, nyx.drawing.lcl, nyx.canvas.geometry, nyx.paint.buffer.lcl;

type
  { Fixture-owned window; the scope borrows it and the window retains no scope
    owner. A callback may revoke publication while its raw window remains alive. }
  TPaintProbe = class(TCustomControl)
  protected
    procedure Paint; override;
    procedure EraseBackground(ADC: HDC); override;
  public
    Paints: Integer;
    RevokeNext: Boolean;
    SkipErase: Boolean;
    InspectInitial: Boolean;
    SawMemoryDC: Boolean;
    InitialColor: TColor;
    Scope: INyxLCLPaintBuffer;
    procedure UseBuffering(AEnabled: Boolean);
  end;
  { Joined worker only attempts forbidden native scope operations. The UI owns
    its stable control, retains the scope and waits before retiring either. No
    worker creates a handle, changes a control or dispatches a UI callback. }
  TWrongThread = class(TThread)
  private
    FControl: TWinControl;
    FScope: INyxLCLPaintBuffer;
  protected
    procedure Execute; override;
  public
    AttachRefused: Boolean;
    RetireRefused: Boolean;
    constructor Create(AControl: TWinControl; const AScope: INyxLCLPaintBuffer);
  end;
  { The application borrows this fixture-owned exception receiver only while
    the native windows exist. Errors are recorded and raised at ordinary checks. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

var
  GChecks: Integer;
  GFailure: TFailure;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if (GFailure.Error <> '') or not ACondition then
  begin
    raise Exception.Create('Native buffer: ' + AMessage + ' / ' + GFailure.Error);
  end;
  Inc(GChecks);
end;

procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Error := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
end;

procedure TPaintProbe.UseBuffering(AEnabled: Boolean);
begin
  DoubleBuffered := AEnabled;
end;

procedure TPaintProbe.Paint;
begin
  inherited Paint;
  Inc(Paints);

  if InspectInitial then
  begin
    SawMemoryDC := WindowFromDC(Canvas.Handle) = 0;
    InitialColor := ColorToRGB(Canvas.Pixels[300, 112]);
  end;

  if RevokeNext then
  begin
    RevokeNext := False;
    Scope.Retire;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlue;
  Canvas.FillRect(ClientRect);
end;

procedure TPaintProbe.EraseBackground(ADC: HDC);
begin

  if not SkipErase then
  begin
    inherited EraseBackground(ADC);
  end;
end;

constructor TWrongThread.Create(AControl: TWinControl; const AScope: INyxLCLPaintBuffer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FControl := AControl;
  FScope := AScope;
end;

procedure TWrongThread.Execute;
var
  LUnexpected: INyxLCLPaintBuffer;
begin
  try
    LUnexpected := AttachNyxLCLPaintBuffer(FControl);
  except
    on E: EInvalidOp do
    begin
      AttachRefused := True;
    end;
  end;
  try
    FScope.Retire;
  except
    on E: EInvalidOp do
    begin
      RetireRefused := True;
    end;
  end;
end;

{ Independent Comctl32 client. The fixture borrows this stack counter until its
  explicit removal; the Nyx adapter must retain this callback and forward once. }
function OtherSubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; AID: UINT_PTR; AReference: DWORD_PTR): LRESULT; stdcall;
begin

  if AMessage = WM_PAINT then
  begin
    Inc(PInteger(Pointer(AReference))^);
  end;
  Result := DefSubclassProc(AWindow, AMessage, AWParam, ALParam);
end;

procedure Run;
var
  LForm: TForm;
  LHost: TPanel;
  LProbe: TPaintProbe;
  LMemo: TMemo;
  LDrawing: TNyxLCLDrawing;
  LBitmap: TBitmap;
  LTheme: TNyxTheme;
  LWorker: TWrongThread;
  LScope: INyxLCLPaintBuffer;
  LNewScope: INyxLCLPaintBuffer;
  LDuplicate: INyxLCLPaintBuffer;
  LBefore: TNyxLCLPaintBufferStats;
  LAfter: TNyxLCLPaintBufferStats;
  LPaints: Integer;
  LOther: Integer;
  LOtherBefore: Integer;
  LIndex: Integer;
  LGDIBefore: DWORD;
  LGDIAfter: DWORD;
  LClip: TRect;
  LFailed: Boolean;
  LHandle: HWND;
begin
  LForm := TForm.CreateNew(nil);
  LBitmap := TBitmap.Create;
  LTheme := TNyxTheme.Create;
  LOther := 0;
  LProbe := nil;
  try
    LForm.SetBounds(24, 24, 420, 320);
    LHost := TPanel.Create(LForm);
    LHost.Parent := LForm;
    LHost.Align := alClient;
    LHost.BevelOuter := bvNone;
    LHost.DoubleBuffered := True;
    LProbe := TPaintProbe.Create(LForm);
    LProbe.Parent := LHost;
    LProbe.SetBounds(-64, -48, 2400, 1800);
    LProbe.UseBuffering(True);
    LMemo := TMemo.Create(LProbe);
    LMemo.Parent := LProbe;
    LMemo.SetBounds(80, 64, 160, 64);
    LMemo.Text := 'An edited note worth keeping';
    LForm.Show;
    Application.ProcessMessages;
    LProbe.HandleNeeded;
    LMemo.SetFocus;
    LMemo.SelStart := 4;
    LMemo.SelLength := 3;
    LHandle := LProbe.Handle;
    Check(SetWindowSubclass(LHandle, @OtherSubclass, 7, DWORD_PTR(@LOther)),
      'independent native subclass installs before the Nyx scope');
    LScope := AttachNyxLCLPaintBuffer(LProbe);
    LProbe.Scope := LScope;
    Check((LScope <> nil) and LScope.Stats.Attached, 'scope attaches to the live owned HWND');
    LPaints := LProbe.Paints;
    LOtherBefore := LOther;
    LProbe.Repaint;
    LAfter := LScope.Stats;
    Check((LProbe.Paints = LPaints + 1) and (LOther = LOtherBefore + 1),
      'one native paint delivers one application callback and preserves the other client');
    Check((LAfter.BufferedPaints > 0) and (LAfter.FailedCommits = 0),
      'actual native paint publishes its viewport buffer');
    Check((LAfter.Width > 0) and (LAfter.Height > 0) and
      (LAfter.Width <= LHost.ClientWidth) and (LAfter.Height <= LHost.ClientHeight),
      'allocation is bounded by visible pixels instead of the 2400x1800 control');
    Check((LMemo.Text = 'An edited note worth keeping') and (LMemo.SelStart = 4) and
      (LMemo.SelLength = 3) and LMemo.Focused,
      'real child editing, focus and selection survive parent buffering');
    LProbe.SkipErase := True;
    LProbe.InspectInitial := True;
    LProbe.Repaint;
    Check(LProbe.SawMemoryDC and
      (LProbe.InitialColor = ColorToRGB(TColor(GetSysColor(COLOR_BTNFACE)))),
      'declined background erasure retains the normal initialized cache color');
    LProbe.InspectInitial := False;
    LProbe.SkipErase := False;
    LClip := Types.Rect(300, 112, 332, 132);
    LPaints := LProbe.Paints;
    Windows.InvalidateRect(LHandle, @LClip, True);
    Windows.UpdateWindow(LHandle);
    LAfter := LScope.Stats;
    Check((LAfter.Width <= 32) and (LAfter.Height <= 20) and
      (LProbe.Paints = LPaints + 1), 'partial dirty input allocates only its actual rectangle');
    LBefore := LAfter;
    LProbe.UseBuffering(False);
    LProbe.Repaint;
    Check(LScope.Stats.BufferedPaints = LBefore.BufferedPaints,
      'caller opt-out retains the normal native unbuffered path');
    LProbe.UseBuffering(True);
    LProbe.Repaint;
    Check(LScope.Stats.BufferedPaints > LBefore.BufferedPaints,
      'caller reenablement resumes the same managed scope');
    LBefore := LScope.Stats;
    LBitmap.SetSize(64, 64);
    Windows.SendMessage(LHandle, WM_PAINT, WPARAM(LBitmap.Canvas.Handle), 0);
    Check((LScope.Stats.BufferedPaints = LBefore.BufferedPaints) and
      (ColorToRGB(LBitmap.Canvas.Pixels[16, 16]) = ColorToRGB(clBlue)),
      'supplied-DC painting preserves the existing direct memory result');
    LFailed := False;
    try
      LDuplicate := AttachNyxLCLPaintBuffer(LProbe);
    except
      on E: EArgumentException do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LScope.Stats.Attached, 'duplicate ownership refuses without replacing the live scope');
    LWorker := TWrongThread.Create(LProbe, LScope);
    try
      LWorker.Start;
      LWorker.WaitFor;
      Check(LWorker.AttachRefused and LWorker.RetireRefused and LScope.Stats.Attached,
        'joined workers cannot install or retire native UI callbacks');
    finally
      LWorker.Free;
    end;
    LProbe.Repaint;
    LGDIBefore := GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
    for LIndex := 1 to 16 do
    begin
      LProbe.Repaint;
    end;
    LGDIAfter := GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
    Check((LGDIBefore > 0) and (LGDIAfter = LGDIBefore),
      'repeated viewport paints release their temporary native DCs and bitmaps');
    RemoveWindowSubclass(LHandle, @OtherSubclass, 7);
    LProbe.Scope := nil;
    RecreateWnd(LProbe);
    LProbe.HandleNeeded;
    Check(not LScope.Stats.Attached, 'native handle replacement automatically retires the old lease');
    LNewScope := AttachNyxLCLPaintBuffer(LProbe);
    LProbe.Scope := LNewScope;
    LProbe.Repaint;
    Check(LNewScope.Stats.Attached and (LNewScope.Stats.BufferedPaints > 0),
      'new native handles admit an independent scope');
    LBefore := LNewScope.Stats;
    LPaints := LProbe.Paints;
    LProbe.RevokeNext := True;
    LProbe.Repaint;
    LAfter := LNewScope.Stats;
    Check(not LAfter.Attached and (LAfter.BufferedPaints = LBefore.BufferedPaints) and
      (LProbe.Paints = LPaints + 1), 'callback retirement revokes the pending native publication');
    LNewScope.Retire;
    LScope.Retire;
    LProbe.Scope := nil;
    LProbe.Visible := False;
    LDrawing := TNyxLCLDrawing.Create(LForm);
    LDrawing.Parent := LHost;
    LDrawing.SetBounds(0, 0, 2400, 1800);
    LDrawing.Configure(NewNyxDrawingPlan(NyxCanvasBounds(0, 0, 2400, 1800))
      .Path(NyxDrawingRectangle(NyxCanvasBounds(10, 10, 80, 60)),
        NyxDrawingStyle.Fill(NyxDrawingPaint(NyxRGB(20, 60, 100))))
      .Build, nil, LTheme, 'Bounded native drawing');
    LDrawing.Repaint;
    LAfter := LDrawing.PaintBufferStats;
    Check(LAfter.Attached and (LAfter.BufferedPaints > 0) and
      (LAfter.Width <= LHost.ClientWidth) and (LAfter.Height <= LHost.ClientHeight),
      'ordinary Nyx drawings use the same bounded native buffer contract');
    RecreateWnd(LDrawing);
    LDrawing.HandleNeeded;
    LDrawing.Repaint;
    Check(LDrawing.PaintBufferStats.Attached and (LDrawing.PaintBufferStats.BufferedPaints > 0),
      'ordinary Nyx drawings renew their scope after framework handle recreation');
  finally

    if (LProbe <> nil) and LProbe.HandleAllocated then
    begin
      RemoveWindowSubclass(LProbe.Handle, @OtherSubclass, 7);
    end;

    if LNewScope <> nil then
    begin
      LNewScope.Retire;
    end;

    if LScope <> nil then
    begin
      LScope.Retire;
    end;
    LTheme.Free;
    LBitmap.Free;
    LForm.Free;
  end;
  Check(not LScope.Stats.Attached and not LNewScope.Stats.Attached,
    'retained diagnostic interfaces grant no access to retired controls');
end;

begin
  try
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    try
      Run;
    finally
      Application.OnException := nil;
      GFailure.Free;
    end;
    WriteLn('PASS ', GChecks, ' actual native viewport-buffer checks; no capture saved');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
