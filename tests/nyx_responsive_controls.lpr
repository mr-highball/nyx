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
program nyx_responsive_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.presentations, nyx.responsive
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Classes, Interfaces, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl{$endif};

var
  GDocument: TNyxDocument;
  GPlain: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  {$ifdef NYX_MANUAL_CONSUMER}GCapability: INyxPresentationView;{$endif}
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GReplacement: TJSHTMLElement;
  GInput: TJSHTMLTextAreaElement;
  GStep: Integer;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GForm: TForm;
  GHost: TPanel;
  GInput: TMemo;
  GReplacement: TPanel;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Responsive control: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifdef NYX_MANUAL_CONSUMER}
procedure ManualChoices;
var
  LRejected: Boolean;
  LOther: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  LHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TPanel{$endif};

  function DirectionIsColumn(ARenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}): Boolean;
  begin
    {$ifdef PAS2JS}
    Result := window.getComputedStyle(ARenderer.ElementFor('workspace')).getPropertyValue('flex-direction') = 'column';
    {$else}
    Result := ARenderer.ControlFor('other-editor').Top >
      ARenderer.ControlFor('notes-editor').Top + ARenderer.ControlFor('notes-editor').Height;
    {$endif}
  end;

begin
  GCapability := GRenderer.Presentations;
  GCapability.Select(NyxPresentation('focused'));
  Check(DirectionIsColumn(GRenderer), 'Managed manual selection reaches actual column geometry');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Manual selection retains the actual focused input');
  Check({$ifdef PAS2JS}GInput.value{$else}GInput.Text{$endif} = 'Keep this focused English draft.',
    'Manual selection preserves independent live text');
  Check({$ifdef PAS2JS}(GInput.selectionStart = 5) and (GInput.selectionEnd = 9)
    {$else}(GInput.SelStart = 5) and (GInput.SelLength = 4){$endif}, 'Manual selection preserves the physical text range');
  Check({$ifdef PAS2JS}document.activeElement = GInput{$else}GForm.ActiveControl = GInput{$endif},
    'Manual selection preserves actual input focus');
  LRejected := False;
  try
    GCapability.Select(NyxPresentation('absent'));
  except
    on ENyxPresentation do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and DirectionIsColumn(GRenderer), 'Unknown manual names refuse before changing physical presentation');
  LOther := {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', '800px');
  LHost.style.setProperty('height', '400px');
  document.body.appendChild(LHost);
  {$else}
  LHost := TPanel.Create(GForm);
  LHost.Parent := GForm;
  LHost.BevelOuter := bvNone;
  LHost.SetBounds(0, 0, 800, 400);
  {$endif}
  try
    LOther.Render(GDocument, GDocument.Pages[0], LHost, False);
    Check(not DirectionIsColumn(LOther), 'A second actual view starts with independent automatic defaults');
    LOther.Presentations.Select(NyxPresentation('wide workspace'));
    Check(DirectionIsColumn(GRenderer), 'Another view selection cannot alter the first view geometry');
  finally
    LOther.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
  end;
  GCapability.Select(NyxPresentation('wide workspace'));
  Check(not DirectionIsColumn(GRenderer), 'Another exclusive manual choice replaces the previous layout');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'Manual switches never rewrite authored source data');
  GDocument.Presentations.Define(NyxPresentation('wide workspace'), TNyxViewportCondition.Any.WidthBelow(900));
  Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False), 'Activation replacement uses a retained property refresh');
  Check(GCapability.Connected and not GCapability.Selection.Reference.Defined,
    'Retained managed capability clears a choice deliberately replaced by automatic activation');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Activation replacement retains the same actual input');
  GDocument.Presentations.Define(NyxPresentation('wide workspace'), TNyxPresentationCondition.Manual);
  Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False), 'Restored manual activation preserves mount lifetime');
  GCapability.Automatic;
  Check(not DirectionIsColumn(GRenderer), 'Automatic restores the host configuration after manual switching');
end;
{$endif}

{$ifdef PAS2JS}
procedure Step;
var
  LFirst: TJSHTMLElement;
  LSecond: TJSHTMLElement;
  LRect: TJSDOMRect;
  LOther: TJSDOMRect;
  LExpected: String;
begin
  try
    LExpected := 'column';

    if (GStep = 0) or (GStep = 2) or (GStep = 4) then
    begin
      LExpected := 'row';
    end;
    { Headless virtual clocks may run a timer before the next rendering turn.
      Wait for the real observer/style result, rather than invoking Sync or
      treating an arbitrary timer delay as proof of observer delivery. }

    if window.getComputedStyle(GRenderer.ElementFor('workspace'))
      .getPropertyValue('flex-direction') <> LExpected then
    begin
      Inc(GPolls);

      if GPolls > 80 then
      begin
        raise Exception.Create('Viewport observer did not apply the requested direction');
      end;
      window.setTimeout(@Step, 16);
      Exit;
    end;
    GPolls := 0;
    LFirst := GRenderer.ElementFor('notes-editor');
    LSecond := GRenderer.ElementFor('other-editor');
    LRect := LFirst.getBoundingClientRect;
    LOther := LSecond.getBoundingClientRect;
    case GStep of
      0:
        begin
          Check(Abs(LRect.top - LOther.top) < 2, 'Wide viewport uses a row');
          Check(LOther.left > LRect.left, 'Wide controls have distinct columns');
          GInput := TJSHTMLTextAreaElement(GRenderer.InputFor('notes-editor'));
          GInput.value := 'Keep this focused English draft.';
          GInput.focus;
          GInput.selectionStart := 5;
          GInput.selectionEnd := 9;
          GHost.style.setProperty('width', '390px');
          Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
            'First responsive rule refreshes the retained ordinary view');
        end;
      1:
        begin
          Check(LOther.top > LRect.bottom, 'ResizeObserver applies compact column automatically');
          Check(GRenderer.InputFor('notes-editor') = GInput, 'Compact rule retains the same real input');
          Check(GInput.value = 'Keep this focused English draft.', 'Compact rule retains independent text');
          Check((GInput.selectionStart = 5) and (GInput.selectionEnd = 9), 'Compact rule retains exact range');
          Check(document.activeElement = GInput, 'Compact rule retains focus');
          Check(Abs((LOther.top - LRect.bottom) - 8) < 2, 'Compact gap is actual rendered space');
          Check(Abs(LOther.left - LRect.left) < 2, 'Actual column direction replaces row centering');
          {$ifdef NYX_PRESENTATION_CONSUMER}
          GDocument.Presentations.Define(NyxPresentation('compact'),
            TNyxViewportCondition.Any.WidthBelow(380));
          Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
            'Changed shared definition admits the actual retained browser view');
          Check(window.getComputedStyle(GRenderer.ElementFor('workspace'))
            .getPropertyValue('flex-direction') = 'row', 'Changed shared predicate changes actual browser flow');
          Check(GRenderer.InputFor('notes-editor') = GInput, 'Definition editing retains the actual browser input');
          Check(GInput.value = 'Keep this focused English draft.', 'Definition editing retains live browser text');
          GDocument.Presentations.Define(NyxPresentation('compact'),
            TNyxViewportCondition.Any.WidthBelow(640));
          Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
            'Restored shared definition refreshes without remount');
          Check(window.getComputedStyle(GRenderer.ElementFor('workspace'))
            .getPropertyValue('flex-direction') = 'column', 'Restored definition restores actual browser flow');
          {$endif}
          GHost.style.setProperty('width', '640px');
        end;
      2:
        begin
          Check(Abs(LRect.top - LOther.top) < 2, 'Exclusive 640 boundary restores the wide row');
          Check(GRenderer.InputFor('notes-editor') = GInput, 'Boundary crossing retains input identity');
          Check(GInput.value = 'Keep this focused English draft.', 'Leaving a rule retains text');
          Check(TNyxCodec.Encode(GDocument) = GBefore, 'Automatic resize leaves accepted design untouched');
          GHost.style.setProperty('height', '200px');
        end;
      3:
        begin
          Check(LOther.top > LRect.bottom, 'Height-only observer change activates short landscape column');
          Check(GRenderer.InputFor('notes-editor') = GInput, 'Height rule retains actual input identity');
          Check(GInput.value = 'Keep this focused English draft.', 'Height rule retains live text');
          Check((GInput.selectionStart = 5) and (GInput.selectionEnd = 9), 'Height rule retains range');
          Check(document.activeElement = GInput, 'Height rule retains focus');
          Check(Abs((LOther.top - LRect.bottom) - 6) < 2, 'Short landscape gap reaches rendered geometry');
          GHost.style.setProperty('height', '300px');
        end;
      4:
        begin
          Check(Abs(LRect.top - LOther.top) < 2, 'Exclusive height boundary returns to ordinary row');
          Check(GRenderer.InputFor('notes-editor') = GInput, 'Restored height retains input identity');
          {$ifdef NYX_MANUAL_CONSUMER}ManualChoices;{$endif}
          GReplacement := TJSHTMLElement(document.createElement('div'));
          GReplacement.style.setProperty('width', '390px');
          GReplacement.style.setProperty('height', '420px');
          document.body.appendChild(GReplacement);
          GRenderer.MoveHost(GReplacement);
        end;
      5:
        begin
          Check(LOther.top > LRect.bottom, 'Retained view adopts replacement host width');
          Check(document.activeElement = GInput, 'Host replacement restores retained focus');
          Check(GInput.selectionStart = 5, 'Host replacement retains range');
          Check(GRenderer.TryRefresh(GPlain, GPlain.Pages[0], False),
            'Removing the last responsive rule refreshes the retained view');
          Check(GRenderer.InputFor('notes-editor') = GInput,
            'Last-rule removal retains actual input identity');
          Check(window.getComputedStyle(GRenderer.ElementFor('workspace'))
            .getPropertyValue('flex-direction') = 'row', 'Last-rule removal restores ordinary direction');
          GRenderer.Unmount;
          {$ifdef NYX_MANUAL_CONSUMER}
          Check(not GCapability.Connected, 'Unmount retires caller-retained managed presentation capabilities');
          {$endif}
          GReplacement.style.setProperty('width', '800px');
          GHost.style.setProperty('width', '390px');
          GRenderer.Render(GDocument, GDocument.Pages[0], GHost, False);
        end;
      6:
        begin
          Check(LOther.top > LRect.bottom, 'Fresh mount applies initial compact width');
          Check(GRenderer.InputFor('notes-editor') <> GInput, 'An ended mount creates an independent new input');
          Check(TNyxCodec.Encode(GDocument) = GBefore, 'Unmount/remount retains authored pair meaning');
          document.body.setAttribute('data-nyx-responsive-controls', 'passed');
          document.body.setAttribute('data-nyx-responsive-checks', IntToStr(GChecks));
          Exit;
        end;
    end;
    Inc(GStep);
    window.setTimeout(@Step, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-responsive-controls', 'failed');
      document.body.setAttribute('data-nyx-responsive-error', LException.Message);
    end;
  end;
end;

procedure ObserveCompactHost;
var
  LMarker: String;
begin
  Inc(GPolls);
  LMarker := GFrame.contentDocument.body.getAttribute('data-nyx-responsive-controls');

  if (LMarker = 'passed') or (LMarker = 'failed') then
  begin
    document.body.setAttribute('data-nyx-responsive-controls', LMarker);
    document.body.setAttribute('data-nyx-responsive-checks',
      GFrame.contentDocument.body.getAttribute('data-nyx-responsive-checks'));
    document.body.setAttribute('data-nyx-responsive-error',
      GFrame.contentDocument.body.getAttribute('data-nyx-responsive-error'));
    Exit;
  end;

  if GPolls > 80 then
  begin
    document.body.setAttribute('data-nyx-responsive-controls', 'failed');
    document.body.setAttribute('data-nyx-responsive-error', 'Compact frame did not finish');
    Exit;
  end;
  window.setTimeout(@ObserveCompactHost, 100);
end;

{$else}
procedure Pump;
begin
  Application.ProcessMessages;
  Application.ProcessMessages;
end;

procedure Capture(const AName: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LBitmap.SetSize(GForm.Width, GForm.Height);
    GForm.PaintTo(LBitmap.Canvas, GForm.Left, GForm.Top);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure NativeJourney;
var
  LFirst: TControl;
  LSecond: TControl;
begin
  GForm.ClientWidth := 800;
  GForm.ClientHeight := 480;
  GForm.Show;
  Pump;
  GRenderer.Render(GPlain, GPlain.Pages[0], GHost, False);
  Pump;
  LFirst := GRenderer.ControlFor('notes-editor');
  LSecond := GRenderer.ControlFor('other-editor');
  Check(LFirst.Top = LSecond.Top, 'Wide viewport uses a row');
  Check(LSecond.Left > LFirst.Left, 'Wide controls have distinct columns');
  GInput := TMemo(GRenderer.InputFor('notes-editor'));
  GInput.Text := 'Keep this focused English draft.';
  GInput.SetFocus;
  GInput.SelStart := 5;
  GInput.SelLength := 4;
  Capture('responsive-wide.png');
  GForm.ClientWidth := 390;
  Pump;
  Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
    'First responsive rule refreshes the retained ordinary native view');
  Pump;
  Check(LSecond.Top > LFirst.Top + LFirst.Height, 'Native OnResize applies compact column');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Compact rule retains actual memo identity');
  Check(GInput.Text = 'Keep this focused English draft.', 'Compact rule retains input text');
  Check(GInput.SelStart = 5, 'Compact rule retains caret');
  Check(GInput.SelLength = 4, 'Compact rule retains selected range');
  Check(GForm.ActiveControl = GInput, 'Compact rule retains focus');
  Check(LSecond.Top - LFirst.Top - LFirst.Height = 10, 'Concrete native compact gap wins');
  Check(LSecond.Left = LFirst.Left, 'Automatic column aligns fixed widths at their leading edge');
  {$ifdef NYX_PRESENTATION_CONSUMER}
  GDocument.Presentations.Define(NyxPresentation('compact'),
    TNyxViewportCondition.Any.WidthBelow(380));
  Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
    'Changed shared definition admits the actual retained native view');
  Pump;
  Check(LFirst.Top = LSecond.Top, 'Changed shared predicate changes actual native flow');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Definition editing retains the native memo');
  Check(GInput.Text = 'Keep this focused English draft.', 'Definition editing retains live native text');
  GDocument.Presentations.Define(NyxPresentation('compact'),
    TNyxViewportCondition.Any.WidthBelow(640));
  Check(GRenderer.TryRefresh(GDocument, GDocument.Pages[0], False),
    'Restored shared definition refreshes without remount');
  Pump;
  Check(LSecond.Top > LFirst.Top + LFirst.Height, 'Restored definition restores actual native flow');
  {$endif}
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'Native resize does not rewrite accepted design');
  Capture('responsive-compact.png');
  GForm.ClientWidth := 640;
  Pump;
  Check(LFirst.Top = LSecond.Top, 'Exclusive boundary restores the wide row');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Leaving compact retains memo identity');
  Check(GInput.Text = 'Keep this focused English draft.', 'Leaving compact retains current live text');
  { Keep the available width outside the native-specific compact rule even
    when vertical overflow introduces a widgetset scrollbar. Height is the
    only dimension changed by the transition under qualification. }
  GForm.ClientWidth := 800;
  Pump;
  GForm.ClientHeight := 200;
  Pump;
  Check(LSecond.Top > LFirst.Top + LFirst.Height, 'Height-only native resize activates short landscape');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Height rule retains native memo identity');
  Check(GInput.Text = 'Keep this focused English draft.', 'Height rule retains native live text');
  Check((GInput.SelStart = 5) and (GInput.SelLength = 4), 'Height rule retains native selection');
  Check(GForm.ActiveControl = GInput, 'Height rule retains native focus');
  Check(LSecond.Top - LFirst.Top - LFirst.Height = 6, 'Short landscape gap reaches native geometry');
  GForm.ClientHeight := 300;
  Pump;
  Check(LFirst.Top = LSecond.Top, 'Exclusive native host height boundary restores row despite scrollbars');
  GForm.ClientHeight := 480;
  Pump;
  Check(LFirst.Top = LSecond.Top, 'Restored native height returns to the ordinary row');
  {$ifdef NYX_MANUAL_CONSUMER}ManualChoices;{$endif}
  GReplacement := TPanel.Create(GForm);
  GReplacement.Parent := GForm;
  GReplacement.SetBounds(0, 0, 390, 460);
  GReplacement.BevelOuter := bvNone;
  GRenderer.MoveHost(GReplacement);
  Pump;
  Check(LSecond.Top > LFirst.Top + LFirst.Height, 'Moved native host applies its own viewport');
  Check(GRenderer.InputFor('notes-editor') = GInput, 'Moved native host retains memo identity');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'Moved native host preserves accepted tree');
  Check(GRenderer.TryRefresh(GPlain, GPlain.Pages[0], False),
    'Removing the last responsive rule refreshes the native view');
  Check(GRenderer.InputFor('notes-editor') = GInput,
    'Native last-rule removal retains actual input identity');
  Check(LFirst.Top = LSecond.Top, 'Native last-rule removal restores ordinary direction');
  GRenderer.Unmount;
  {$ifdef NYX_MANUAL_CONSUMER}
  Check(not GCapability.Connected, 'Unmount retires caller-retained managed presentation capabilities');
  {$endif}
  Check(GRenderer.Root = nil, 'Native unmount retires the responsive tree');
  WriteLn('PASS ', GChecks, ' actual native responsive controls');
end;
{$endif}

{ Derive an independently owned baseline from the unchanged compiled MCP view.
  Removing only explicit wire rules lets actual target Refresh qualify the first
  and last responsive rule, without modifying the accepted companion document. }
procedure PreparePlainBaseline;
var
  LRow: TNyxNode;
  LIndex: Integer;
begin
  GPlain := GDocument.Clone;
  LRow := GPlain.Find('workspace');
  for LIndex := LRow.Props.Count - 1 downto 0 do
  begin

    if (Copy(LRow.Props.Names[LIndex], 1, 14) = '@nyx.viewport:') or
      (Copy(LRow.Props.Names[LIndex], 1, 19) = '@nyx.viewport-size:') or
      (Copy(LRow.Props.Names[LIndex], 1, 18) = '@nyx.presentation:') then
    begin
      LRow.Props.Delete(LIndex);
    end;
  end;
end;

begin
  GDocument := BuildNyxDocument;
  PreparePlainBaseline;
  GBefore := TNyxCodec.Encode(GDocument);
  GChecks := 0;
  {$ifdef PAS2JS}
  if window.location.search = '?host=1' then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.setProperty('width', '390px');
    GFrame.style.setProperty('height', '700px');
    GFrame.style.setProperty('border', '0');
    GFrame.src := window.location.pathname;
    document.body.appendChild(GFrame);
    window.setTimeout(@ObserveCompactHost, 100);
    Exit;
  end;
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '800px');
  GHost.style.setProperty('height', '420px');
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GPlain, GPlain.Pages[0], GHost, False);
  GStep := 0;
  window.setTimeout(@Step, 100);
  {$else}
  Application.Initialize;
  { Automated qualification must report widget callback failures to the process,
    instead of leaving a hidden LCL exception dialog awaiting human dismissal. }
  Application.CaptureExceptions := False;
  GForm := TForm.Create(nil);
  GRenderer := TNyxLCLRenderer.Create;
  try
    GForm.Caption := 'Room for ideas';
    GHost := TPanel.Create(GForm);
    GHost.Parent := GForm;
    GHost.Align := alClient;
    GHost.BevelOuter := bvNone;
    NativeJourney;
  finally
    GRenderer.Free;
    GPlain.Free;
    GDocument.Free;
    GForm.Free;
  end;
  {$endif}
end.
