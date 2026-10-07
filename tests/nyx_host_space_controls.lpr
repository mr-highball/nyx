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
program nyx_host_space_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.hostspace, nyx.responsive, nyx.modal,
  {$ifdef PAS2JS}
  JS, Web, nyx.model, nyx.controls, nyx.render.browser,
  nyx.hostspace.browser, nyx.modal.browser;
  {$else}
  Interfaces, Forms, Controls, nyx.hostspace.lcl;
  {$endif}

type
  { Supplied metric observations qualify the shared admission/dispatch boundary;
    actual target handlers below qualify their separate receiver lifetime. This
    fixture does not claim a real keyboard, pinch gesture or visual event source. }
  TMetricObserver = class(TNyxHostSpaceObserver)
  public
    Next: TNyxHostSpaceSnapshot;
    constructor Create;
  protected
    function Capture: TNyxHostSpaceSnapshot; override;
  end;
  TReceiver = class
  public
    Calls: Integer;
    ExistingCalls: Integer;
    ReplacementCalls: Integer;
    Retire: Boolean;
    Last: TNyxHostExtent;
    procedure Changed(const AExtent: TNyxHostExtent);
    {$ifndef PAS2JS}
    procedure ExistingResize(ASender: TObject);
    procedure ReplacementResize(ASender: TObject);
    {$endif}
  end;

var
  GChecks: Integer;
  GSpace: INyxHostSpace;
  GReceiver: TReceiver;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Host space: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TMetricObserver.Create;
begin
  inherited Create;
  Next := NyxHostSpace(390, 640, 640, 1);
  Initialize(NyxHostSizing.Fit(nhfAvailableHeight), Next);
end;

function TMetricObserver.Capture: TNyxHostSpaceSnapshot;
begin
  Result := Next;
end;

procedure TReceiver.Changed(const AExtent: TNyxHostExtent);
begin
  Inc(Calls);
  Last := AExtent;

  if Retire then
  begin
    GSpace.Disconnect;
    GSpace := nil;
    { The copied payload remains safe after releasing the owning observation. }
    Check(Last.Width > 0, 'callback payload survives observation retirement');
  end;
end;

{$ifndef PAS2JS}
procedure TReceiver.ExistingResize(ASender: TObject);
begin
  Inc(ExistingCalls);
end;

procedure TReceiver.ReplacementResize(ASender: TObject);
begin
  Inc(ReplacementCalls);
end;
{$endif}

procedure Refusals;
var
  LIndex: Integer;
  LRefused: Boolean;
  LSnapshot: TNyxHostSpaceSnapshot;
  LExtent: TNyxHostExtent;
begin
  for LIndex := 0 to 8 do
  begin
    LRefused := False;
    try
      case LIndex of
        0: LSnapshot := NyxHostSpace(-1, 640, 640, 1);
        1: LSnapshot := NyxHostSpace(390, -1, 640, 1);
        2: LSnapshot := NyxHostSpace(390, 640, -1, 1);
        3: LSnapshot := NyxHostSpace(390, 640, 640, 0);
        4: LSnapshot := NyxHostSpace(NaN, 640, 640, 1);
        5: LSnapshot := NyxHostSpace(390, Infinity, 640, 1);
        6: LSnapshot := NyxHostSpace(390, 640, 640, Infinity);
        7: LSnapshot := NyxHostSpace(2147483648.0, 640, 640, 1);
        8: LSnapshot := Default(TNyxHostSpaceSnapshot);
      end;
      LExtent := LSnapshot.Resolve(NyxHostSizing);
      Check(LExtent.Width >= 0, 'reachable only for an admitted observation');
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid or unobserved metric refuses before publication');
  end;
end;

procedure Geometry;
var
  LLayout: TNyxHostSizingOptions;
  LAvailable: TNyxHostSizingOptions;
  LSnapshot: TNyxHostSpaceSnapshot;
  LExtent: TNyxHostExtent;
begin
  LLayout := NyxHostSizing;
  LAvailable := LLayout.Fit(nhfAvailableHeight);
  Check(LLayout.FitMode = nhfLayout, 'fluent policy leaves its source options unchanged');
  Check(LAvailable.FitMode = nhfAvailableHeight, 'closed fluent available-height policy');
  LSnapshot := NyxHostSpace(390, 640, 360, 1);
  LExtent := LSnapshot.Resolve(LLayout);
  Check((LExtent.Width = 390) and (LExtent.Height = 640), 'layout sizing remains available');
  LExtent := LSnapshot.Resolve(LAvailable);
  Check((LExtent.Width = 390) and (LExtent.Height = 360), 'visual-only occlusion fits height');
  Check(TNyxViewportCondition.Any.WidthBelow(400).Matches(LExtent.Width, LExtent.Height),
    'ordinary typed width presentation consumes the fitted allocation');
  LExtent := NyxHostSpace(390, 640, 320, 2).Resolve(LAvailable);
  Check((LExtent.Width = 390) and (LExtent.Height = 640), 'pinch zoom preserves allocation');
  LExtent := NyxHostSpace(390, 640, 180, 2).Resolve(LAvailable);
  Check((LExtent.Width = 390) and (LExtent.Height = 360), 'magnified occlusion normalizes height');
  LExtent := NyxHostSpace(390, 640, 1000, 1).Resolve(LAvailable);
  Check(LExtent.Height = 640, 'visual observations cannot enlarge the allocated host');
  LExtent := NyxHostSpace(390, 640, 0, 1).Resolve(LAvailable);
  Check(LExtent.Height = 0, 'zero available space is explicit');
  LExtent := NyxHostSpace(0, 0, 640, 1).Resolve(LAvailable);
  Check((LExtent.Width = 0) and (LExtent.Height = 0), 'hidden host remains zero-sized');
  LExtent := NyxHostSpace(390.5, 640, 359.5, 1).Resolve(LAvailable);
  Check((LExtent.Width = 391) and (LExtent.Height = 360), 'logical half-pixel rounding');
  LExtent := NyxHostSpace(2147483647.0, 2147483647.0, 1e308, 1e308).Resolve(LAvailable);
  Check((LExtent.Width = High(Integer)) and (LExtent.Height = High(Integer)),
    'extreme visual metrics clamp without multiplying overflowing values');
  LExtent := NyxHostSpace(390, 640, 1e308, 1e-308).Resolve(LAvailable);
  Check(LExtent.Height = 1, 'very small visual scale retains bounded available height');
  Check(NyxModal('Source').HostFit = nhfLayout, 'modal defaults retain layout policy');
  Check(NyxModal('Source').Sizing(nhfAvailableHeight).HostFit = nhfAvailableHeight,
    'modal opts into the same typed fitting contract');
end;

procedure AdmissionAndLifetime;
var
  LMetrics: TMetricObserver;
  LPrevious: TNyxHostSpaceSnapshot;
  LCount: Integer;
  LRefused: Boolean;
begin
  LMetrics := TMetricObserver.Create;
  GSpace := LMetrics;
  GSpace.OnChange := GReceiver.Changed;
  LCount := GReceiver.Calls;
  Check(GSpace.Connected and (GSpace.Extent.Height = 640), 'initial capture is published silently');
  GSpace.Refresh;
  Check(GReceiver.Calls = LCount, 'equal allocation does not notify');
  LMetrics.Next := NyxHostSpace(390, 640, 320, 2);
  GSpace.Refresh;
  Check(GReceiver.Calls = LCount, 'pure magnification is silent');
  Check(GSpace.Snapshot.VisualScale = 2, 'silent observations still update copied metrics');
  LMetrics.Next := NyxHostSpace(390, 640, 180, 2);
  GSpace.Refresh;
  Check((GReceiver.Calls = LCount + 1) and (GReceiver.Last.Height = 360),
    'visual-only occlusion produces one semantic allocation change');
  LPrevious := GSpace.Snapshot;
  LMetrics.Next := Default(TNyxHostSpaceSnapshot);
  LRefused := False;
  try
    GSpace.Refresh;
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'invalid capture refuses');
  Check((GSpace.Snapshot.VisualHeight = LPrevious.VisualHeight) and
    (GSpace.Extent.Height = 360) and (GReceiver.Calls = LCount + 1),
    'capture failure retains accepted metrics and sends no callback');
  GSpace.Disconnect;
  GSpace.OnChange := GReceiver.Changed;
  LMetrics.Next := NyxHostSpace(390, 640, 640, 1);
  GSpace.Refresh;
  Check(not GSpace.Connected and not Assigned(GSpace.OnChange), 'disconnected receiver cannot reattach');
  Check((GSpace.Extent.Height = 360) and (GReceiver.Calls = LCount + 1),
    'disconnected capture is inert and its last value remains readable');
  GSpace := nil;
  LMetrics := TMetricObserver.Create;
  GSpace := LMetrics;
  GSpace.OnChange := GReceiver.Changed;
  GReceiver.Retire := True;
  LMetrics.Next := NyxHostSpace(390, 640, 360, 1);
  GSpace.Refresh;
  GReceiver.Retire := False;
  Check(GSpace = nil, 'callback can release the managed observation');
end;

{$ifndef PAS2JS}
procedure WatchNative(AHost: TWinControl; AFit: TNyxHostFit);
begin
  { Finish the construction call before testing last-owner release. Native FPC
    retains a function-result interface temporary until its enclosing routine
    exits; Studio likewise constructs its observation in a separate constructor. }
  GSpace := NewNyxLCLHostSpace(AHost, NyxHostSizing.Fit(AFit));
  GSpace.OnChange := GReceiver.Changed;
end;

procedure NativeHost;
var
  LForm: TForm;
  LCount: Integer;
  LOriginal: Integer;
  LExtent: TNyxHostExtent;
  LRefused: Boolean;
begin
  LForm := TForm.CreateNew(nil);
  try
    LForm.ClientWidth := 1200;
    LForm.ClientHeight := 800;
    LForm.OnResize := GReceiver.ExistingResize;
    LForm.Show;
    Application.ProcessMessages;
    LOriginal := GReceiver.ExistingCalls;
    GSpace := NewNyxLCLHostSpace(LForm, NyxHostSizing.Fit(nhfAvailableHeight));
    GSpace.OnChange := GReceiver.Changed;
    Check((GSpace.Extent.Width = LForm.ClientWidth) and
      (GSpace.Extent.Height = LForm.ClientHeight), 'real native client capture');
    LCount := GReceiver.Calls;
    LForm.ClientWidth := 390;
    LForm.ClientHeight := 640;
    Application.ProcessMessages;
    Check(GReceiver.Calls > LCount, 'actual LCL resizing reaches managed host observation');
    Check((GSpace.Extent.Width = 390) and (GSpace.Extent.Height = 640),
      'native compact allocation is exact client space');
    Check(GReceiver.ExistingCalls > LOriginal, 'host original resize receiver is preserved');
    LForm.OnResize := GReceiver.ReplacementResize;
    GSpace.Disconnect;
    GSpace := nil;
    LCount := GReceiver.Calls;
    LOriginal := GReceiver.ReplacementCalls;
    LForm.ClientHeight := 600;
    Application.ProcessMessages;
    Check(GReceiver.ReplacementCalls > LOriginal,
      'disconnect does not overwrite a later host resize receiver');
    Check(GReceiver.Calls = LCount, 'disconnect removes the real native resize handler');
    WatchNative(LForm, nhfLayout);
    GSpace := nil;
    LCount := GReceiver.Calls;
    LForm.ClientHeight := 590;
    Application.ProcessMessages;
    Check(GReceiver.Calls = LCount, 'automatic interface retirement detaches native handlers');
    GSpace := NewNyxLCLHostSpace(LForm, NyxHostSizing.Fit(nhfAvailableHeight));
    GSpace.OnChange := GReceiver.Changed;
    GReceiver.Retire := True;
    LForm.ClientHeight := 580;
    Application.ProcessMessages;
    GReceiver.Retire := False;
    Check(GSpace = nil, 'real resize callback can retire its observing owner');
    GSpace := NewNyxLCLHostSpace(LForm, NyxHostSizing);
    GSpace.OnChange := GReceiver.Changed;
    LExtent := GSpace.Extent;
    LForm.Free;
    LForm := nil;
    Check(not GSpace.Connected and not Assigned(GSpace.OnChange),
      'client destruction cancels the borrowed host and receiver');
    GSpace.Refresh;
    Check(GSpace.Extent.Same(LExtent), 'retired native host retains its last copied allocation');
    GSpace := nil;
  finally
    GSpace := nil;
    LForm.Free;
  end;
  LRefused := False;
  try
    GSpace := NewNyxLCLHostSpace(nil, NyxHostSizing);
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GSpace = nil), 'missing native host refuses before registration');
end;
{$else}
procedure BrowserHost;
var
  LModal: INyxBrowserModalHost;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LRenderer: TNyxBrowserRenderer;
  LInput: TJSHTMLElement;
  LExtent: TNyxHostExtent;
begin
  GSpace := NewNyxBrowserHostSpace(NyxHostSizing.Fit(nhfAvailableHeight));
  LExtent := ReadNyxBrowserHostSpace.Resolve(NyxHostSizing.Fit(nhfAvailableHeight));
  Check(GSpace.Extent.Same(LExtent), 'actual browser metrics reach the public contract');
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxBrowserRenderer.Create;
  LModal := NewNyxBrowserModalHost;
  try
    LPage := NewNyxPage('notes');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxMemo('notes-input').Configure.Text('Notes').Done);
    LRenderer.Render(LDocument, LDocument.Pages[0], LModal.Element);
    LModal.Show(NyxModal('Notes').Sizing(nhfAvailableHeight));
    LInput := LRenderer.ElementFor('notes-input');

    if not (LInput is TJSHTMLTextAreaElement) then
    begin
      LInput := TJSHTMLElement(LInput.querySelector('textarea'));
    end;
    TJSHTMLTextAreaElement(LInput).value := 'An unfinished thought';
    LModal.Show(NyxModal('Notes').Sizing(nhfLayout));
    LModal.Show(NyxModal('Notes').Sizing(nhfAvailableHeight));
    Check(LModal.IsOpen and document.body.contains(LInput), 'policy changes retain mounted modal input');
    Check(TJSHTMLTextAreaElement(LInput).value = 'An unfinished thought',
      'policy changes retain its uncommitted draft');
    Check(LModal.Element.style.getPropertyValue('max-height') = IntToStr(LExtent.Height) + 'px',
      'modal uses resolved available height rather than layout viewport units');
  finally
    LRenderer.Free;
    LPage := nil;
    LDocument.Free;
    LModal := nil;
    GSpace.Disconnect;
    GSpace := nil;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GReceiver := TReceiver.Create;
    try
      Geometry;
      Refusals;
      AdmissionAndLifetime;
      {$ifdef PAS2JS}BrowserHost;{$else}NativeHost;{$endif}
    finally
      GSpace := nil;
      GReceiver.Free;
    end;
    WriteLn('PASS / host space / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'failed');
      document.body.textContent := LException.Message;
      {$else}Halt(1);{$endif}
    end;
  end;
end.
