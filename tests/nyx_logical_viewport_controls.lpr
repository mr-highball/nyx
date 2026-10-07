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
program nyx_logical_viewport_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codegen,
  nyx.viewport, nyx.behavior, nyx.events, nyx.callbacks, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.theme;
  {$else}, Interfaces, Classes, Forms,
  {$ifdef WINDOWS}Windows,{$endif}
  Controls, StdCtrls, ExtCtrls, Types,
  Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl, nyx.widgets.lcl;{$endif}

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  TInputNotification = class external name 'Event' (TJSEvent)
    constructor new(const AName: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  TAccess = class(TControl);
  {$endif}
  { The managed probe retains only event values. Renderer owns each subscription
    and detaches producers before its actual controls are destroyed. }
  TProbe = class(TNyxEventCallback)
    Count: Integer;
    Last: TNyxEventInfo;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GRenderer: TRenderer;
  GDocument: TNyxDocument;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  {$else}GHost: TForm;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Count);
  Last := AEvent.Copy;
end;

function Face(const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}Result := GRenderer.ElementFor(AID, niRuntime);
  {$else}Result := GRenderer.ControlFor(AID, niRuntime);{$endif}
end;

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  Application.Idle(False);
  {$endif}
end;

function OnScreen(AFace: TFace): Boolean;
{$ifdef PAS2JS}
var
  LBounds: TJSDOMRect;
  LHostBounds: TJSDOMRect;
begin
  LBounds := AFace.getBoundingClientRect;
  LHostBounds := GHost.getBoundingClientRect;
  Result := (LBounds.bottom > LHostBounds.top) and (LBounds.top < LHostBounds.bottom) and
    (LBounds.right > LHostBounds.left) and (LBounds.left < LHostBounds.right);
end;
{$else}
var
  LBounds: TPoint;
  LViewport: TNyxViewportSnapshot;
begin
  LBounds := AFace.ClientToScreen(Point(0, 0));
  LBounds := Face('home').Parent.ScreenToClient(LBounds);
  LViewport := GRenderer.ViewViewport;
  Result := (AFace.Height > 0) and (AFace.Width > 0) and
    (LBounds.Y + AFace.Height > 0) and (LBounds.Y < LViewport.Height) and
    (LBounds.X + AFace.Width > 0) and (LBounds.X < LViewport.Width);
end;
{$endif}

{ Full original-size descendants remain independent controls. English review
  text belongs here; existing Unicode fixtures retain broader qualification. }
function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LLabel: INyxLabel;
  LCard: INyxCard;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LScroll: INyxScroll;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LPage.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.AddPage(LPage);
  for LIndex := 0 to 2047 do
  begin
    LLabel := NewNyxLabel('caption-' + IntToStr(LIndex));
    LLabel.WithText('Chapter ' + IntToStr(LIndex + 1));
    LLabel.Configure.Height(32).Done;
    LPage.Add(LLabel);
  end;
  LCard := NewNyxCard('notes-card');
  LCard.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  LPage.Add(LCard);
  LMemo := NewNyxMemo('notes-memo');
  LMemo.WithText('Notes');
  LMemo.Configure.Value('A quiet afternoon').Height(120).Done;
  LCard.Add(LMemo);
  LButton := NewNyxButton('save-button');
  LButton.WithText('Save notes');
  LCard.Add(LButton);
  { These standard native choices have preferred automatic sizes. Keep them
    beyond the initial viewport to qualify first-handle parking as well as
    retained focus/input after Reveal, using the same portable source. }
  LCard.Add(NewNyxCheckbox('review-checkbox').WithText('Email updates')
    .Configure.Value(False).Done);
  LCard.Add(NewNyxSwitch('review-switch').WithText('Remember preferences')
    .Configure.Value(False).Done);
  LCard.Add(NewNyxRadio('review-radio').WithText('Review first')
    .Configure.Value(False).Done);

  LPage := NewNyxPage('nested');
  LPage.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.AddPage(LPage);
  LScroll := NewNyxScroll('nested-scroll');
  LScroll.Configure.Layout(nlColumn).Gap(8).Height(180).Done;
  LPage.Add(LScroll);
  for LIndex := 0 to 2047 do
  begin
    LLabel := NewNyxLabel('nested-caption-' + IntToStr(LIndex));
    LLabel.WithText('Section ' + IntToStr(LIndex + 1));
    LLabel.Configure.Height(32).Done;
    LScroll.Add(LLabel);
  end;
  LMemo := NewNyxMemo('nested-memo');
  LMemo.WithText('Review');
  LMemo.Configure.Height(120).Done;
  LScroll.Add(LMemo);
end;

{$ifndef PAS2JS}
procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount <> 1 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GHost.ClientWidth, GHost.ClientHeight);
    GHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

{$ifndef PAS2JS}
{ Compare settled native allocation with Nyx's physical LCL bounds. Reading only
  TControl.Width/Height misses a late-created HWND retaining its default size. }
procedure CheckNativeAllocation(const AID: TNyxText; AParked: Boolean);
var
  LControl: TWinControl;
  {$ifdef WINDOWS}
  LBounds: Windows.TRect;
  {$endif}
begin
  LControl := TWinControl(Face(AID));
  LControl.HandleNeeded;
  Pump;

  if AParked then
  begin
    Check(LControl.Visible and (LControl.Width = 0) and (LControl.Height = 0),
      'Parked control retains authored visibility with no physical area: ' + AID);
  end;
  {$ifdef WINDOWS}
  Check(Windows.GetWindowRect(LControl.Handle, LBounds),
    'Actual owned native rectangle is available: ' + AID);
  Check((LBounds.Right - LBounds.Left = LControl.Width) and
    (LBounds.Bottom - LBounds.Top = LControl.Height),
    'Actual owned native allocation matches projected bounds: ' + AID);
  {$endif}
end;

procedure CheckRetainedChoices;
const
  CChoices: array[0..2] of TNyxText =
    ('review-checkbox', 'review-switch', 'review-radio');
var
  LIndex: Integer;
  LControl: TWinControl;
begin
  for LIndex := Low(CChoices) to High(CChoices) do
  begin
    LControl := GRenderer.FocusFor(CChoices[LIndex]);
    GRenderer.FocusFor('save-button').SetFocus;
    GRenderer.ScrollView(0, 0);
    CheckNativeAllocation(CChoices[LIndex], True);
    LControl.SetFocus;
    Pump;
    WriteLn('CHOICE ', CChoices[LIndex], ' focused=', LControl.Focused,
      ' screen=', OnScreen(Face(CChoices[LIndex])), ' bounds=', LControl.Left, ',',
      LControl.Top, ',', LControl.Width, ',', LControl.Height,
      ' scroll=', GRenderer.ViewViewport.Y.Position:0:0);
    Check((GRenderer.FocusFor(CChoices[LIndex]) = LControl) and
      LControl.Focused and OnScreen(Face(CChoices[LIndex])),
      'Actual choice focus reveals its original retained control: ' + CChoices[LIndex]);
    CheckNativeAllocation(CChoices[LIndex], False);
    {$ifdef WINDOWS}
    { Native button-message input reaches the owned widget's normal producer.
      This qualifies programmatic host input, not physical hardware. }
    Windows.SendMessage(LControl.Handle, BM_CLICK, 0, 0);
    Pump;

    if LControl is TRadioButton then
    begin
      Check(TRadioButton(LControl).Checked, 'Actual revealed radio accepts native activation');
    end
    else
    begin
      Check(TCheckBox(LControl).Checked, 'Actual revealed check/switch accepts native activation');
    end;
    GRenderer.ScrollView(0, 0);
    GRenderer.Reveal(CChoices[LIndex]);
    Pump;

    if LControl is TRadioButton then
    begin
      Check(TRadioButton(LControl).Checked, 'Radio value survives parking and reveal');
    end
    else
    begin
      Check(TCheckBox(LControl).Checked, 'Check/switch value survives parking and reveal');
    end;
    {$endif}
  end;
end;
{$endif}

procedure Run;
var
  LSource: TNyxText;
  LIndex: Integer;
  LFirst: TFace;
  LLast: TFace;
  LBefore: TNyxViewportSnapshot;
  LAfter: TNyxViewportSnapshot;
  LProbe: TProbe;
  LCallback: INyxEventCallback;
  LSubscription: INyxEventSubscription;
  LRefused: Boolean;
  {$ifdef PAS2JS}LMemo: TJSHTMLTextAreaElement;
  LStyle: TJSHTMLElement;
  LEvent: TJSEvent;
  LTheme: TNyxTheme;
  {$else}LMemo: TMemo;
  LScroll: TScrollBox;
  LBar: TScrollBar;
  LRejected: TNyxDocument;
  LRejectedPage: INyxPage;
  LRejectedMemo: INyxMemo;
  LRejectedSplit: INyxSplitView;
  LRefusalReason: TNyxText;{$endif}
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := Fixture;
  GRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '300px');
  GHost.style.setProperty('width', '640px');
  GHost.style.setProperty('overflow', 'auto');
  document.body.appendChild(GHost);
  LStyle := TJSHTMLElement(document.createElement('style'));
  LTheme := TNyxTheme.Create;
  try
    LStyle.textContent := LTheme.CSS;
  finally
    LTheme.Free;
  end;
  document.head.appendChild(LStyle);
  {$else}
  GHost := TForm.Create(nil);
  GHost.ClientWidth := 640;
  GHost.ClientHeight := 300;
  GHost.Show;
  {$endif}
  try
    GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
    Pump;
    LSource := TNyxCodegen.Generate(GDocument);
    LBefore := GRenderer.ViewViewport;
    Check(LBefore.Y.Extent > 81963, 'Complete 2048-control extent includes the bottom compound');
    Check(LBefore.X.Extent <= LBefore.Width,
      'Scrollbar appearance settles width without introducing false horizontal overflow');
    {$ifndef PAS2JS}
    CheckNativeAllocation('notes-card', True);
    CheckNativeAllocation('notes-memo', True);
    CheckNativeAllocation('review-checkbox', True);
    CheckNativeAllocation('review-switch', True);
    CheckNativeAllocation('review-radio', True);
    CheckRetainedChoices;
    GRenderer.ScrollView(0, 0);
    {$endif}
    LFirst := Face('caption-0');
    LLast := Face('caption-2047');
    for LIndex := 0 to 2047 do
    begin
      GRenderer.Reveal('caption-' + IntToStr(LIndex), niRuntime);
      {$ifndef PAS2JS}

      if not OnScreen(Face('caption-' + IntToStr(LIndex))) then
      begin
        LAfter := GRenderer.ViewViewport;
        WriteLn('Unreachable ', LIndex, ' native top ', Face('caption-' + IntToStr(LIndex)).Top,
          ' height ', Face('caption-' + IntToStr(LIndex)).Height,
          ' screen ', Face('caption-' + IntToStr(LIndex)).ClientToScreen(Point(0, 0)).Y,
          ' root screen ', Face('home').ClientToScreen(Point(0, 0)).Y,
          ' host screen ', Face('home').Parent.ClientToScreen(Point(0, 0)).Y,
          ' viewport ', LAfter.Y.Position:0:0, '/', LAfter.Height:0:0);
      end;
      {$endif}
      Check(OnScreen(Face('caption-' + IntToStr(LIndex))),
        'Exact descendant is physically reachable ' + IntToStr(LIndex));
      {$ifndef PAS2JS}
      Check(TLabel(Face('caption-' + IntToStr(LIndex))).Caption =
        'Chapter ' + IntToStr(LIndex + 1), 'Every retained caption keeps its complete value');
      {$endif}
    end;
    Check((Face('caption-0') = LFirst) and (Face('caption-2047') = LLast),
      'Scrolling retains both original endpoint controls');

    GRenderer.Reveal('notes-memo', niDesign);
    Pump;
    Check(OnScreen(Face('notes-memo')), 'Bottom compound input is on screen');
    {$ifdef PAS2JS}
    LMemo := TJSHTMLTextAreaElement(Face('notes-memo').querySelector('textarea'));
    LMemo.focus;
    LMemo.value := 'Notes for tomorrow';
    LEvent := TInputNotification.new('input', TJSObject.new);
    LMemo.dispatchEvent(LEvent);
    LMemo.selectionStart := 5;
    LMemo.selectionEnd := 5;
    {$else}
    LMemo := TMemo(GRenderer.InputFor('notes-memo'));
    LMemo.SetFocus;
    LMemo.Text := 'Notes for tomorrow';
    LMemo.SelStart := 5;
    LMemo.SelLength := 0;
    Check(LMemo.Focused, 'Distant native memo receives actual keyboard focus');
    {$endif}
    LBefore := GRenderer.ViewViewport;
    GRenderer.Select('notes-memo');
    GRenderer.Select('home');
    LAfter := GRenderer.ViewViewport;
    Check(LBefore.SamePosition(LAfter), 'Outline-only selection cannot jump the viewport');

    { A clipped oversized page preserves original pointer coordinates. }
    LProbe := TProbe.Create;
    LCallback := LProbe;
    LSubscription := GRenderer.Events.OnPointerDown(NyxControlEvents('home')).Subscribe(LCallback);
    {$ifndef PAS2JS}
    TAccess(Face('home')).MouseDown(mbLeft, [], 8, 10);
    Check((LProbe.Count = 1) and LProbe.Last.HasPointer and
      (LProbe.Last.Pointer.Y = LBefore.Y.Position + 10),
      'Actual projected native page producer reports original logical pointer position');
    {$endif}
    LSubscription.Cancel;
    LSubscription := nil;
    LCallback := nil;

    {$ifndef PAS2JS}Capture('logical-viewport-bottom');{$endif}
    {$ifdef PAS2JS}GHost.style.setProperty('width', '390px');
    {$else}GHost.ClientWidth := 390;{$endif}
    Pump;
    GRenderer.Reveal('notes-memo');
    {$ifndef PAS2JS}
    Capture('logical-viewport-390');

    if not OnScreen(Face('notes-memo')) then
    begin
      LAfter := GRenderer.ViewViewport;
      WriteLn('Narrow memo bounds ', Face('notes-memo').BoundsRect.Top, '/',
        Face('notes-memo').Height, ' card ', Face('notes-card').Top, '/',
        Face('notes-card').Height, ' root ', Face('home').Top, '/', Face('home').Height,
        ' view ', LAfter.Y.Position:0:0, '/', LAfter.Y.Extent:0:0, '/', LAfter.Height:0:0);
      WriteLn('Widths/left memo ', Face('notes-memo').Left, '/', Face('notes-memo').Width,
        ' card ', Face('notes-card').Left, '/', Face('notes-card').Width,
        ' root ', Face('home').Left, '/', Face('home').Width,
        ' view ', LAfter.X.Position:0:0, '/', LAfter.X.Extent:0:0, '/', LAfter.Width:0:0,
        ' actual memo ', Face('notes-memo').ClientToScreen(Point(0, 0)).X, '/',
        Face('notes-memo').ClientToScreen(Point(0, 0)).Y,
        ' actual root ', Face('home').ClientToScreen(Point(0, 0)).X, '/',
        Face('home').ClientToScreen(Point(0, 0)).Y);
    end;
    {$endif}
    Check(OnScreen(Face('notes-memo')), 'Narrow resize preserves access to the same bottom input');
    {$ifdef PAS2JS}
    Check((TJSHTMLTextAreaElement(Face('notes-memo').querySelector('textarea')) = LMemo) and
      (LMemo.value = 'Notes for tomorrow') and (LMemo.selectionStart = 5),
      'Narrow browser geometry retains actual input, draft and caret');
    {$else}
    Check((GRenderer.InputFor('notes-memo') = LMemo) and
      (LMemo.Text = 'Notes for tomorrow') and LMemo.Focused and (LMemo.SelStart = 5),
      'Narrow native geometry retains actual input, draft, focus and caret');
    Capture('logical-viewport-390');
    {$endif}

    { Actual focus entry reveals a parked zero-area control before application
      focus observers run. No explicit Reveal substitutes for this check. }
    GRenderer.Reveal('caption-0');
    {$ifndef PAS2JS}
    GRenderer.FocusFor('save-button').SetFocus;
    GRenderer.ScrollView(0, 0);
    Check(Face('notes-memo').Height = 0, 'Offscreen memo is retained outside physical allocation');
    Check(not LMemo.Focused, 'Focus-entry fixture first leaves the actual memo');
    LMemo.SetFocus;
    Pump;
    Check(OnScreen(Face('notes-memo')) and LMemo.Focused and (LMemo.SelStart = 5),
      'Actual focus reveals the retained distant input without losing its caret');
    {$endif}
    Check(TNyxCodegen.Generate(GDocument) = LSource,
      'Scrolling, selection, draft input and resize leave authored source unchanged');

    LRefused := False;
    try
      GRenderer.Reveal('missing-control', niRuntime);
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Missing exact identity refuses without replacing the live view');

    GRenderer.Render(GDocument, GDocument.Pages[1], GHost);
    Pump;
    GRenderer.Reveal('nested-memo');
    Pump;
    LAfter := GRenderer.ViewportFor('nested-scroll');
    Check((LAfter.Y.Extent > 81920) and (LAfter.Y.Position > 80000),
      'Independent nested scroll scope retains its complete logical extent and reveals the input');
    {$ifndef PAS2JS}
    LScroll := TScrollBox(GRenderer.ControlFor('nested-scroll'));
    LMemo := TMemo(GRenderer.InputFor('nested-memo'));
    LMemo.SetFocus;
    LMemo.Text := 'Nested notes';
    Check((LMemo.Text = 'Nested notes') and LMemo.Focused and (LScroll.Height = 180),
      'Actual nested native memo edits inside the authored bounded scroll control');
    LProbe := TProbe.Create;
    LCallback := LProbe;
    LSubscription := GRenderer.Events.OnScroll(NyxControlEvents('nested-scroll'))
      .Subscribe(LCallback);
    LBefore := GRenderer.ViewportFor('nested-scroll');
    LBar := nil;
    for LIndex := 0 to LScroll.ControlCount - 1 do
    begin

      if (LScroll.Controls[LIndex] is TScrollBar) and
        (TScrollBar(LScroll.Controls[LIndex]).Kind = sbVertical) then
      begin
        LBar := TScrollBar(LScroll.Controls[LIndex]);
        Break;
      end;
    end;
    Check(LBar <> nil, 'Logical scrolling reuses an actual standard LCL scrollbar');
    LBar.Position := Trunc(LBefore.Y.Position) - 24;
    Pump;
    Check((LProbe.Count = 1) and LProbe.Last.HasViewport and
      (LProbe.Last.Viewport.Y.Position = LBefore.Y.Position - 24),
      'Actual standard scrollbar changes reach the typed coalesced viewport event');
    TAccess(TControl(LScroll)).DoMouseWheel([], 120, Point(8, 8));
    Pump;
    Check(GRenderer.ViewportFor('nested-scroll').Y.Position = LBefore.Y.Position - 48,
      'Native wheel default moves the actual logical scrollbar');
    LSubscription.Cancel;
    LSubscription := nil;
    LCallback := nil;

    { A target gap is an explicit candidate refusal, never a cropped giant
      memo pretending to preserve its native text layout. Keep the accepted
      controls/source while staging and disposing this invalid candidate. }
    LRejected := TNyxDocument.Create;
    try
      LRejectedPage := NewNyxPage('rejected');
      LRejectedMemo := NewNyxMemo('oversized-memo');
      LRejectedMemo.Configure.Height(70000).Done;
      LRejectedPage.Add(LRejectedMemo);
      LRejected.AddPage(LRejectedPage);
      LRefused := False;
      try
        GRenderer.Render(LRejected, LRejected.Pages[0], GHost);
      except
        on LException: ENyxModel do
        begin
          LRefused := Pos('logical paint adapter', LException.Message) > 0;
        end;
      end;
      Check(LRefused and (GRenderer.InputFor('nested-memo') = LMemo) and
        (GRenderer.Root.ID = 'nested'),
        'Unsupported giant native face refuses while retaining the accepted view');
    finally
      LRejectedMemo := nil;
      LRejectedPage := nil;
      LRejected.Free;
    end;

    { A legal window height can still put a stacked separator/pane beyond the
      signed native position domain. Admission must refuse before arranging
      those hosts, with the accepted input and view still alive. }
    LRejected := TNyxDocument.Create;
    try
      LRejectedPage := NewNyxPage('rejected-split-page');
      LRejectedSplit := NewNyxSplitView('oversized-split');
      LRejectedSplit.Configure.Height(40000).SplitOrientation(nsoStacked)
        .SplitMaximum(90).SplitPosition(90).Done;
      LRejectedSplit.Add(NewNyxColumn('rejected-first-pane'));
      LRejectedSplit.Add(NewNyxColumn('rejected-second-pane'));
      LRejectedPage.Add(LRejectedSplit);
      LRejected.AddPage(LRejectedPage);
      LRefused := False;
      LRefusalReason := '';
      try
        GRenderer.Render(LRejected, LRejected.Pages[0], GHost);
      except
        on LException: ENyxModel do
        begin
          LRefusalReason := LException.Message;
          LRefused := Pos('logical pane projection', LException.Message) > 0;
        end;
      end;
      Check(LRefused and (GRenderer.InputFor('nested-memo') = LMemo) and
        (GRenderer.Root.ID = 'nested'),
        'Unsupported distant split pane refuses before native positioning and retains the view: ' +
        LRefusalReason);
    finally
      LRejectedSplit := nil;
      LRejectedPage := nil;
      LRejected.Free;
    end;
    {$endif}
    GRenderer.Unmount;
    Pump;
    LRefused := False;
    try
      LAfter := GRenderer.ViewViewport;
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Retired view refuses containing viewport observation');
  finally
    GRenderer.Free;
    GRenderer := nil;
    {$ifdef PAS2JS}GHost.remove;
    LStyle.remove;{$else}GHost.Free;{$endif}
    GDocument.Free;
    GDocument := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logical-controls', 'passed');
    document.body.setAttribute('data-logical-checks', IntToStr(GChecks));
    {$else}WriteLn('PASS ', GChecks, ' actual logical viewport/control checks');{$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logical-controls', 'failed');
      document.body.setAttribute('data-logical-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
