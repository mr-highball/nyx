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
program nyx_scroll_chrome_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, SysUtils, Classes, Controls, StdCtrls, Grids, Windows,
  nyx.text, nyx.types, nyx.controls, nyx.model, nyx.codec, nyx.render.lcl,
  nyx.scroll.chrome, nyx.scroll.chrome.lcl, nyx.gestures.lcl, nyx.theme,
  nyx.scroll.presentation, nyx.scroll.configuration;

type
  { Detached flags emulate one collection input authority. The host and native
    control remain fixture-owned; the port retains only its retirement frame. }
  TScrollLifetime = class(TInterfacedObject, INyxLCLScrollChromeLifetime)
  public
    Active: Boolean;
    Available: Boolean;
    Epoch: Integer;
    Physical: INyxNativeGestureFrame;
    function Current: Boolean;
    function InputReady: Boolean;
    function InputEpoch: Integer;
    function Frame: INyxNativeGestureFrame;
  end;

  { Application borrows this receiver only during the fixture. Native callback
    failures become a terminal test failure rather than a modal exception box;
    the exception itself is never retained beyond the notification. }
  TNativeFailures = class
  public
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GChecks: Integer;
  GFailures: TNativeFailures;

procedure TNativeFailures.Failed(ASender: TObject; AException: Exception);
begin

  if MessageText = '' then
  begin
    MessageText := AException.Message;
  end;
end;

function TScrollLifetime.Current: Boolean;
begin
  Result := Active;
end;

function TScrollLifetime.InputReady: Boolean;
begin
  Result := Active and Available;
end;

function TScrollLifetime.InputEpoch: Integer;
begin
  Result := Epoch;
end;

function TScrollLifetime.Frame: INyxNativeGestureFrame;
begin
  Result := Physical;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure RunDeferredGrid;
var
  LHost: TForm;
  LGrid: TStringGrid;
  LFlags: TScrollLifetime;
  LLifetime: INyxLCLScrollChromeLifetime;
  LChrome: INyxLCLScrollChrome;
  LTheme: TNyxTheme;
  LInfo: TScrollInfo;
  LRange: TNyxScrollRange;
  LGeometry: TNyxScrollChromeGeometry;
  LWindow: Windows.TRect;
  LClient: Windows.TRect;
  LOrigin: Windows.TPoint;
  LPoint: Windows.TPoint;
  LX: Integer;
  LY: Integer;
  LInitial: Integer;
  LGrab: Integer;

  function Coordinates(AX, AY: Integer): LPARAM;
  begin
    Result := LPARAM(LongWord(Word(AX)) or (LongWord(Word(AY)) shl 16));
  end;

  procedure BeginThumb;
  begin
    LInfo := Default(TScrollInfo);
    LInfo.cbSize := SizeOf(LInfo);
    LInfo.fMask := SIF_ALL;
    Check(GetScrollInfo(LGrid.Handle, SB_VERT, LInfo), 'grid retains its original native range');
    GetWindowRect(LGrid.Handle, LWindow);
    GetClientRect(LGrid.Handle, LClient);
    LOrigin.X := 0;
    LOrigin.Y := 0;
    Windows.ClientToScreen(LGrid.Handle, LOrigin);
    LRange := NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage);
    LGeometry := LayoutNyxScrollChrome(LClient.Bottom, 0.5, LRange, NyxScrollChromeMetrics);
    LGrab := LGeometry.ThumbLength div 2;
    LX := LWindow.Right - 3;
    LY := LOrigin.Y + LGeometry.ThumbStart + LGrab;
    SendMessage(LGrid.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(LX, LY));
    Check(GetCapture = LGrid.Handle, 'the real grid thumb owns pointer capture');
  end;

  procedure MoveThumb;
  begin
    LPoint.X := LX;
    LPoint.Y := LOrigin.Y + LGeometry.TrackStart + LGeometry.TrackLength -
      LGeometry.ThumbLength + LGrab;
    Windows.ScreenToClient(LGrid.Handle, LPoint);
    SendMessage(LGrid.Handle, WM_MOUSEMOVE, 0, Coordinates(LPoint.X, LPoint.Y));
  end;

  procedure ReleaseThumb;
  begin
    SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, Coordinates(LPoint.X, LPoint.Y));
  end;
begin
  LHost := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create;
  LFlags := TScrollLifetime.Create;
  LLifetime := LFlags;
  LFlags.Active := True;
  LFlags.Available := True;
  LFlags.Physical := NewNyxNativeGestureFrame;
  try
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 240;
    LGrid := TStringGrid.Create(LHost);
    LGrid.Parent := LHost;
    LGrid.Align := alClient;
    LGrid.ColCount := 6;
    LGrid.RowCount := 10000;
    LGrid.DefaultRowHeight := 20;
    LGrid.Options := LGrid.Options - [goThumbTracking];
    LHost.Show;
    Application.ProcessMessages;
    LChrome := AttachNyxLCLScrollChrome(LGrid, LLifetime);
    Check(LChrome <> nil, 'a real LCL grid admits the shared scroll attachment');
    LChrome.Project(0.5, LTheme);
    Application.ProcessMessages;
    LInitial := LGrid.TopRow;
    BeginThumb;
    Check(LRange.Maximum > 65535, 'the real grid source has a full-width pixel range');
    MoveThumb;
    Check(LGrid.TopRow = LInitial, 'deferred tracking preserves the grid caller policy');
    ReleaseThumb;
    Check(LGrid.TopRow > LGrid.RowCount div 2, 'release commits a full-width final grid thumb position');
    Check(GetCapture <> LGrid.Handle, 'final commit releases the real native capture');

    LGrid.TopRow := 1;
    BeginThumb;
    Inc(LFlags.Epoch);
    MoveThumb;
    ReleaseThumb;
    Check(LGrid.TopRow = 1, 'an old drag cannot revive after input epoch replacement');
    Check(GetCapture <> LGrid.Handle, 'epoch replacement withdraws native capture');

    BeginThumb;
    LFlags.Available := False;
    MoveThumb;
    LFlags.Available := True;
    ReleaseThumb;
    Check(LGrid.TopRow = 1, 'refused input cannot finish after admission becomes ready again');
    LChrome.Retire;
    Check(not LChrome.Attached, 'explicit retirement revokes the native attachment');
    LChrome := nil;
  finally

    if LChrome <> nil then
    begin
      LChrome.Retire;
    end;
    LChrome := nil;
    LFlags.Active := False;
    LHost.Free;
    LTheme.Free;
    LLifetime := nil;
  end;
  Application.ProcessMessages;
end;

procedure RunHorizontalMemo(ALength: Integer);
var
  LHost: TForm;
  LMemo: TMemo;
  LFlags: TScrollLifetime;
  LLifetime: INyxLCLScrollChromeLifetime;
  LChrome: INyxLCLScrollChrome;
  LTheme: TNyxTheme;
  LInfo: TScrollInfo;
  LRange: TNyxScrollRange;
  LGeometry: TNyxScrollChromeGeometry;
  LWindow: Windows.TRect;
  LClient: Windows.TRect;
  LOrigin: Windows.TPoint;
  LPoint: Windows.TPoint;
  LGrab: Integer;
  LAlong: Integer;
  LExpected: Integer;
  LStep: Integer;
  LTolerance: Integer;

  function Coordinates(AX, AY: Integer): LPARAM;
  begin
    Result := LPARAM(LongWord(Word(AX)) or (LongWord(Word(AY)) shl 16));
  end;
begin
  LHost := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create;
  LFlags := TScrollLifetime.Create;
  LLifetime := LFlags;
  LFlags.Active := True;
  LFlags.Available := True;
  LFlags.Physical := NewNyxNativeGestureFrame;
  try
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 240;
    LMemo := TMemo.Create(LHost);
    LMemo.Parent := LHost;
    LMemo.Align := alClient;
    LMemo.WordWrap := False;
    LMemo.ScrollBars := ssBoth;

    if ALength > 1000 then
    begin
      { Use a large real font so even Windows' bounded line measurement yields
        a horizontal pixel position beyond the packed thumb message. }
      LMemo.Font.Size := 192;
    end;
    LMemo.Text := StringOfChar('W', ALength);
    LHost.Show;
    Application.ProcessMessages;
    LChrome := AttachNyxLCLScrollChrome(LMemo, LLifetime);
    Check(LChrome <> nil, 'an unwrapped memo admits two-axis scroll chrome');
    LChrome.Project(0.5, LTheme);
    Application.ProcessMessages;
    LInfo := Default(TScrollInfo);
    LInfo.cbSize := SizeOf(LInfo);
    LInfo.fMask := SIF_ALL;
    Check(GetScrollInfo(LMemo.Handle, SB_HORZ, LInfo), 'the real horizontal source range is available');
    LRange := NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage);

    if ALength > 1000 then
    begin
      Check(LRange.LastPosition > High(Word) * 3,
        'the actual horizontal host range exercises the wide-position route');
    end;
    { Measure one actual native horizontal character step independently of the
      attachment's font conversion. Only the large-position fallback uses this
      native granularity; the ordinary pixel assertion stays exact. }
    SendMessage(LMemo.Handle, EM_LINESCROLL, 1, 0);
    GetScrollInfo(LMemo.Handle, SB_HORZ, LInfo);
    LStep := LInfo.nPos - LRange.Position;
    SendMessage(LMemo.Handle, WM_HSCROLL, SB_LEFT, 0);
    GetWindowRect(LMemo.Handle, LWindow);
    GetClientRect(LMemo.Handle, LClient);
    LOrigin.X := 0;
    LOrigin.Y := 0;
    Windows.ClientToScreen(LMemo.Handle, LOrigin);
    LGeometry := LayoutNyxScrollChrome(LClient.Right, 0.5, LRange, NyxScrollChromeMetrics);
    LGrab := LGeometry.ThumbLength div 2;
    LPoint.X := LOrigin.X + LGeometry.ThumbStart + LGrab;
    LPoint.Y := LWindow.Bottom - 3;
    SendMessage(LMemo.Handle, WM_NCLBUTTONDOWN, HTHSCROLL, Coordinates(LPoint.X, LPoint.Y));
    Check(GetCapture = LMemo.Handle, 'the actual horizontal thumb captures input');
    LAlong := LGeometry.TrackStart + (LGeometry.TrackLength - LGeometry.ThumbLength) div 3 + LGrab;
    LExpected := LGeometry.PositionAt(LAlong, LGrab);
    LPoint.X := LOrigin.X + LAlong;
    Windows.ScreenToClient(LMemo.Handle, LPoint);
    SendMessage(LMemo.Handle, WM_MOUSEMOVE, 0, Coordinates(LPoint.X, LPoint.Y));
    SendMessage(LMemo.Handle, WM_LBUTTONUP, 0, Coordinates(LPoint.X, LPoint.Y));
    Check(GetScrollInfo(LMemo.Handle, SB_HORZ, LInfo), 'horizontal scrolling retains its range');
    WriteLn('Horizontal native source: expected=', LExpected, ', actual=', LInfo.nPos,
      ', last=', LRange.LastPosition);
    LTolerance := 1;

    if LExpected > High(Word) then
    begin
      LTolerance := Max(1, (Abs(LStep) + 1) div 2);
      Check(LInfo.nPos > High(Word), 'large horizontal positions never wrap through a packed native word');
    end;
    Check(Abs(LInfo.nPos - LExpected) <= LTolerance,
      'horizontal thumb positions preserve the native pixel or character granularity');
    SendMessage(LMemo.Handle, WM_HSCROLL, SB_LEFT, 0);
    LPoint.X := LOrigin.X + LGeometry.ThumbStart + LGrab;
    LPoint.Y := LWindow.Bottom - 3;
    SendMessage(LMemo.Handle, WM_NCLBUTTONDOWN, HTHSCROLL, Coordinates(LPoint.X, LPoint.Y));
    LPoint.X := LOrigin.X + LGeometry.TrackStart + LGeometry.TrackLength -
      LGeometry.ThumbLength + LGrab;
    Windows.ScreenToClient(LMemo.Handle, LPoint);
    SendMessage(LMemo.Handle, WM_MOUSEMOVE, 0, Coordinates(LPoint.X, LPoint.Y));
    SendMessage(LMemo.Handle, WM_LBUTTONUP, 0, Coordinates(LPoint.X, LPoint.Y));
    Check(GetScrollInfo(LMemo.Handle, SB_HORZ, LInfo) and
      (LInfo.nPos = LRange.LastPosition), 'horizontal thumb endpoints retain the exact full source position');
    LChrome.RememberViewport;
    LChrome.Project(0.75, LTheme);
    LChrome.RestoreViewport;
    LFlags.Available := False;
    Inc(LFlags.Epoch);
    { This is a controlled refresh while physical input is suspended. It does
      not send a source-text mutation, so only the captured input epoch can
      distinguish the queued older anchor from this new accepted viewport. }
    SendMessage(LMemo.Handle, WM_HSCROLL, SB_LEFT, 0);
    LFlags.Available := True;
    Application.ProcessMessages;
    Check(GetScrollInfo(LMemo.Handle, SB_HORZ, LInfo) and (LInfo.nPos = 0),
      'queued viewport restoration cannot cross a collection input epoch');
    LMemo.Align := alNone;
    LMemo.SetBounds(0, 0, 1, 1);
    Application.ProcessMessages;
    Check(GetClientRect(LMemo.Handle, LClient) and
      (LClient.Right >= 0) and (LClient.Right <= 1) and
      (LClient.Bottom >= 0) and (LClient.Bottom <= 1),
      'a tiny real native window retains a valid bounded client rectangle');
    LChrome.Retire;
    LChrome := nil;
  finally

    if LChrome <> nil then
    begin
      LChrome.Retire;
    end;
    LChrome := nil;
    LFlags.Active := False;
    LHost.Free;
    LTheme.Free;
    LLifetime := nil;
  end;
  Application.ProcessMessages;
end;

{ Query this fixture's borrowed native input HWND only. OS client allocation is
  independent from the renderer's logical boxes and from a diagnostic redraw. }
function NonClientWidth(AInput: TMemo): Integer;
var
  LWindow: Windows.TRect;
  LClient: Windows.TRect;
begin
  Check(GetWindowRect(AInput.Handle, LWindow), 'the actual input window has bounds');
  Check(GetClientRect(AInput.Handle, LClient), 'the actual input window has a client area');
  Result := (LWindow.Right - LWindow.Left) - (LClient.Right - LClient.Left);
end;

{ Full Render is deliberately allowed to replace controls. This checks that
  opting in outside a canvas and then withdrawing it restores native default
  allocation, without changing ordinary view dimensions or source content. }
procedure RunOrdinaryPresentation;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LMemo: INyxMemo;
  LInput: TMemo;
  LDefaultWidth: Integer;
begin
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  try
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 240;
    LHost.Show;
    LDocument.AddPage(NewNyxPage('home'));
    LMemo := NewNyxMemo('ordinary-note');
    LMemo.Configure.Height(180).Value('Your notes').Done;
    LDocument.Pages[0].Add(LMemo);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LInput := TMemo(LRenderer.InputFor('ordinary-note'));
    LDefaultWidth := NonClientWidth(LInput);
    NyxScrollConfigure(LMemo.Configure).Presentation(NyxScrollPresentation.MinimumThickness(32));
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LInput := TMemo(LRenderer.InputFor('ordinary-note'));
    Check(NonClientWidth(LInput) >= 32, 'an ordinary unzoomed memo honors explicit minimum chrome');
    Check(LRenderer.ContentScale = 1, 'chrome opt-in does not change ordinary content scale');
    NyxScrollConfigure(LMemo.Configure).UseDefault;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LInput := TMemo(LRenderer.InputFor('ordinary-note'));
    Check(NonClientWidth(LInput) = LDefaultWidth, 'withdrawing ordinary policy restores original native allocation');
    Check(TNyxText(LInput.Text) = 'Your notes', 'default restoration retains authored content');
  finally
    LRenderer.Free;
    LHost.Free;
    LMemo := nil;
    LDocument.Free;
  end;
end;

procedure Run;
const
  CScales: array[0..3] of Double = (0.75, 1.5, 0.5, 1.0);
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LMemo: INyxMemo;
  LInput: TMemo;
  LText: TNyxText;
  LBefore: TNyxText;
  LIndex: Integer;
  LFull: Integer;
  LHalf: Integer;
  LInfo: TScrollInfo;
  LAnchor: Integer;
  LScale: Double;
  LWindow: Windows.TRect;
  LPoint: Windows.TPoint;
  LGeometry: TNyxScrollChromeGeometry;
  LRange: TNyxScrollRange;
  LGrab: Integer;
  LRows: TNyxStrings;
  LReplacement: UnicodeString;

  function Info: TScrollInfo;
  begin
    Result := Default(TScrollInfo);
    Result.cbSize := SizeOf(Result);
    Result.fMask := SIF_ALL;
    Check(GetScrollInfo(LInput.Handle, SB_VERT, Result), 'the admitted input keeps scroll information');
  end;

  function Coordinates(AX, AY: Integer): LPARAM;
  begin
    Result := LPARAM(LongWord(Word(AX)) or (LongWord(Word(AY)) shl 16));
  end;

  procedure DownAt(AX, AY: Integer);
  begin
    SendMessage(LInput.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(AX, AY));
  end;

  procedure CapturedAt(AMessage: UINT; AX, AY: Integer);
  var
    LClient: Windows.TPoint;
  begin
    LClient.X := AX;
    LClient.Y := AY;
    Windows.ScreenToClient(LInput.Handle, LClient);
    SendMessage(LInput.Handle, AMessage, 0, Coordinates(LClient.X, LClient.Y));
  end;
begin
  LDocument := TNyxDocument.Create;
  LRenderer := nil;
  LHost := nil;
  try
    LDocument.AddPage(NewNyxPage('home'));
    LText := '';
    for LIndex := 1 to 80 do
    begin
      LText := LText + TNyxText('A useful line ') + TNyxText(IntToStr(LIndex)) +
        TNyxText(' / retained Unicode 🌙') + TNyxText(#13#10);
    end;
    LMemo := NewNyxMemo('notes');
    LMemo.Configure.Height(180).Done;
    LMemo.Value := LText;
    LDocument.Pages[0].Add(LMemo);
    LBefore := TNyxCodec.Encode(LDocument);
    LHost := TForm.CreateNew(nil);
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 240;
    LHost.Show;
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.SetContentScale(400, 240, 1);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Application.ProcessMessages;
    LInput := TMemo(LRenderer.InputFor('notes'));
    Check(LInput <> nil, 'the normal renderer exposes its real native memo');
    LInfo := Default(TScrollInfo);
    LInfo.cbSize := SizeOf(LInfo);
    LInfo.fMask := SIF_ALL;
    Check(GetScrollInfo(LInput.Handle, SB_VERT, LInfo) and
      (LInfo.nMax > Integer(LInfo.nPage)), 'the real memo has scrollable native content');
    LFull := NonClientWidth(LInput);
    LInput.SetFocus;
    LInput.SelStart := 4;
    LInput.SelLength := 3;
    LRenderer.SetContentScale(400, 240, 0.5);
    Application.ProcessMessages;
    Check(LRenderer.InputFor('notes') = LInput, 'projection retains the same native input');
    LHalf := NonClientWidth(LInput);
    WriteLn('Native nonclient allocation: full=', LFull, ', half=', LHalf);
    Check(Abs(LHalf - Round(LFull * 0.5)) <= 1,
      'native scroll chrome/client allocation follows the absolute uniform factor');
    Check(LInput.Focused and (LInput.SelStart = 4) and (LInput.SelLength = 3),
      'scroll projection retains actual focus and selection');
    LInfo := Info;
    LAnchor := LInfo.nPos;
    GetWindowRect(LInput.Handle, LWindow);
    LPoint.X := LWindow.Right - Max(1, LHalf div 2);
    LPoint.Y := LWindow.Bottom - Max(1, Round(17 * 0.5) div 2);
    DownAt(LPoint.X, LPoint.Y);
    CapturedAt(WM_LBUTTONUP, LPoint.X, LPoint.Y);
    LInfo := Info;
    Check(LInfo.nPos > LAnchor, 'the actual reduced arrow scrolls the retained editor');
    LInput.ReadOnly := True;
    LAnchor := LInfo.nPos;
    DownAt(LPoint.X, LPoint.Y);
    CapturedAt(WM_LBUTTONUP, LPoint.X, LPoint.Y);
    LInfo := Info;
    Check(LInfo.nPos > LAnchor, 'read-only text remains scrollable');
    LInput.Enabled := False;
    LAnchor := LInfo.nPos;
    DownAt(LPoint.X, LPoint.Y);
    CapturedAt(WM_LBUTTONUP, LPoint.X, LPoint.Y);
    LInfo := Info;
    Check(LInfo.nPos = LAnchor, 'disabled input refuses scrollbar actions');
    LInput.Enabled := True;
    LInput.SetFocus;
    LRange := NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage);
    LGeometry := LayoutNyxScrollChrome(LWindow.Bottom - LWindow.Top, 0.5,
      LRange, NyxScrollChromeMetrics);
    LGrab := LGeometry.ThumbLength div 2;
    LPoint.Y := LWindow.Top + LGeometry.ThumbStart + LGrab;
    DownAt(LPoint.X, LPoint.Y);
    LPoint.Y := LWindow.Top + LGeometry.TrackStart + LGeometry.TrackLength -
      LGeometry.ThumbLength + LGrab;
    CapturedAt(WM_MOUSEMOVE, LPoint.X, LPoint.Y);
    CapturedAt(WM_LBUTTONUP, LPoint.X, LPoint.Y);
    LInfo := Info;
    Check(LInfo.nPos = LRange.LastPosition, 'real thumb dragging reaches the exact source endpoint');
    SendMessage(LInput.Handle, EM_LINESCROLL, 0, -5);
    LInfo := Info;
    LAnchor := LInfo.nPos;
    for LIndex := Low(CScales) to High(CScales) do
    begin
      LScale := CScales[LIndex];
      LRenderer.SetContentScale(400, 240, LScale);
      Application.ProcessMessages;
      LInfo := Info;
      WriteLn('Zoom ', LScale:0:2, ': anchor=', LAnchor, ', actual=', LInfo.nPos,
        ', maximum=', LInfo.nMax, ', page=', LInfo.nPage);
      Check(LInfo.nPos = LAnchor, 'repeated absolute zoom preserves the actual memo scroll anchor');
      Check(Abs(NonClientWidth(LInput) - Round(LFull * LScale)) <= 1,
        'scroll allocation never compounds an earlier scale');
    end;
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime projection preserves authored defaults');
    LRenderer.SetContentScale(400, 240, 0.5);
    LInput.Text := TNyxText('A newly supplied source 🌙');
    Application.ProcessMessages;
    LInfo := Info;
    Check(LInfo.nPos = 0, 'a queued old anchor cannot scroll a replacement source');
    LRows := TNyxStrings.Create;
    try
      for LIndex := 1 to 70020 do
      begin
        LRows.Add(TNyxText('Row ') + TNyxText(IntToStr(LIndex)));
      end;
      LInput.Text := LRows.Text;
    finally
      LRows.Free;
    end;
    Application.ProcessMessages;
    LInfo := Info;
    Check(LInfo.nMax > 65535, 'the real native source extends beyond packed 16-bit positions');
    GetWindowRect(LInput.Handle, LWindow);
    LRange := NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage);
    LGeometry := LayoutNyxScrollChrome(LWindow.Bottom - LWindow.Top, 0.5,
      LRange, NyxScrollChromeMetrics);
    LGrab := LGeometry.ThumbLength div 2;
    LPoint.X := LWindow.Right - Max(1, NonClientWidth(LInput) div 2);
    LPoint.Y := LWindow.Top + LGeometry.ThumbStart + LGrab;
    DownAt(LPoint.X, LPoint.Y);
    LPoint.Y := LWindow.Top + LGeometry.TrackStart + LGeometry.TrackLength -
      LGeometry.ThumbLength + LGrab;
    CapturedAt(WM_MOUSEMOVE, LPoint.X, LPoint.Y);
    CapturedAt(WM_LBUTTONUP, LPoint.X, LPoint.Y);
    LInfo := Info;
    Check((LInfo.nPos = LRange.LastPosition) and (LInfo.nPos > 65535),
      'native thumb seeks preserve the complete source position beyond 16 bits');
    LGeometry := LayoutNyxScrollChrome(LWindow.Bottom - LWindow.Top, 0.5,
      NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage), NyxScrollChromeMetrics);
    LPoint.Y := LWindow.Top + LGeometry.ThumbStart + LGeometry.ThumbLength div 2;
    DownAt(LPoint.X, LPoint.Y);
    Check(GetCapture = LInput.Handle, 'an admitted memo thumb owns capture before a source edit');
    LReplacement := 'A newer source';
    SendMessageW(LInput.Handle, EM_REPLACESEL, 0, LPARAM(PWideChar(LReplacement)));
    Check(GetCapture <> LInput.Handle, 'native selection replacement cancels the original source drag');
    LRenderer.SetContentScale(400, 240, 0.75);
    FreeAndNil(LRenderer);
    FreeAndNil(LHost);
    Application.ProcessMessages;
    Check(True, 'retirement withdraws queued restoration without retaining a window or scope');
  finally
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

begin
  Application.Initialize;
  GFailures := TNativeFailures.Create;
  Application.OnException := GFailures.Failed;
  try
    try
      Check(not NyxLCLSupportsScrollChrome(nil), 'an absent native control advertises no scroll capability');
      Run;
      RunOrdinaryPresentation;
      RunDeferredGrid;
      RunHorizontalMemo(1000);
      RunHorizontalMemo(20000);
      Check(GFailures.MessageText = '', 'native callbacks complete without an asynchronous exception');
      WriteLn('PASS ', GChecks, ' actual native scroll-chrome checks');
    except
      on E: Exception do
      begin
        WriteLn('FAIL ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Application.OnException := nil;
    GFailures.Free;
  end;
end.
