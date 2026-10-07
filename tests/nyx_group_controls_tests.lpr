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
program nyx_group_controls_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codegen,
  nyx.events, nyx.callbacks, nyx.scheduler, nyx.behavior,
  nyx.designer.placement, nyx.designer.resize
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.theme;
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Types, LCLIntf,
  nyx.render.lcl, nyx.test.capture.lcl;{$endif}

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  TControlAccess = class(TControl);
  { A managed registration retains this probe; its copied values do not retain
    the renderer, widget or document. Cancellation precedes physical teardown. }
  TPointerProbe = class(TNyxEventCallback)
  public
    Count: Integer;
    Last: TNyxEventInfo;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$endif}

var
  GChecks: Integer;
  GRenderer: TRenderer;
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

procedure Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  Application.Idle(False);
  {$endif}
end;

{$ifndef PAS2JS}
procedure TPointerProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Count);
  Last := AEvent.Copy;
end;

{ Actual LCL pointer producers use client coordinates, but portable positions,
  designer hit faces and screen conversion share the complete outer face. }
procedure CheckPointerMapping(const AID: TNyxText);
var
  LProbe: TPointerProbe;
  LCallback: INyxEventCallback;
  LSubscription: INyxEventSubscription;
  LOrigin: TPoint;
  LActual: TPoint;
  LPoint: TNyxResizePoint;
  LFrame: TNyxDropFrame;
begin
  LProbe := TPointerProbe.Create;
  LCallback := LProbe;
  LSubscription := GRenderer.Events.OnPointerDown(NyxControlEvents(AID)).Subscribe(LCallback);
  try
    TControlAccess(GRenderer.ControlFor(AID)).MouseDown(mbLeft, [], 8, 10);
    Check((LProbe.Count = 1) and LProbe.Last.HasPointer,
      'Actual group pointer producer reaches its managed registration');
    LActual := GRenderer.ControlFor(AID).ClientToScreen(Point(8, 10));
    LPoint := GRenderer.ScreenPointFor(AID, LProbe.Last.Pointer);
    Check((LPoint.X = LActual.X) and (LPoint.Y = LActual.Y),
      'Group portable pointer converts back to its actual native screen position');
    LOrigin := GRenderer.ControlFor(AID).Parent.ClientToScreen(
      Point(GRenderer.ControlFor(AID).Left, GRenderer.ControlFor(AID).Top));
    LFrame := GRenderer.DropFrameFor(AID);
    Check((LFrame.Face.Left = LOrigin.X) and (LFrame.Face.Top = LOrigin.Y),
      'Group designer hit face begins at the outer frame, above its client origin');
  finally
    LSubscription.Cancel;
    LSubscription := nil;
    LCallback := nil;
  end;
end;
{$endif}

function Face(const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}Result := GRenderer.ElementFor(AID);
  {$else}Result := GRenderer.ControlFor(AID);{$endif}
end;

{ The document owns both pages and their independent group controls. Inner padding
  means usable content space, rather than space underneath a group caption or
  frame. This public Pascal fixture qualifies physical adapter behavior; it does
  not replace or mutate the active semantic Studio project. }
function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LGroup: INyxGroup;
  LNested: INyxGroup;
begin
  Result := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LPage.Configure.Layout(nlColumn).Padding(12).Gap(8).Done;
  Result.AddPage(LPage);
  LGroup := NewNyxGroup('settings');
  LGroup.WithText('Project settings').Configure.Layout(nlColumn).Padding(12).Gap(8).Done;
  LPage.Add(LGroup);
  LGroup.Add(NewNyxMemo('notes').WithText('Notes').Configure.Height(120)
    .Value('A quiet afternoon').Done);
  LGroup.Add(NewNyxCheckbox('updates').WithText('Email updates')
    .Configure.Value(False).Done);
  LNested := NewNyxGroup('preferences');
  LNested.WithText('Preferences').Configure.Layout(nlColumn).Padding(8).Gap(8).Done;
  LGroup.Add(LNested);
  LNested.Add(NewNyxMemo('details').WithText('Details').Configure.Height(100).Done);

  LPage := NewNyxPage('layout-options');
  LPage.Configure.Layout(nlColumn).Padding(12).Gap(8).Done;
  Result.AddPage(LPage);
  LGroup := NewNyxGroup('row-settings');
  LGroup.WithText('Quick actions').Configure.Layout(nlRow).Padding(8).Gap(8).Done;
  LPage.Add(LGroup);
  LGroup.Add(NewNyxLabel('row-first').WithText('First').Configure.Width(80).Height(28).Done);
  LGroup.Add(NewNyxLabel('row-second').WithText('Second').Configure.Width(80).Height(28).Done);
  LGroup := NewNyxGroup('grid-settings');
  LGroup.WithText('Grid options').Configure.Layout(nlGrid).Columns(2).Padding(8).Gap(8).Done;
  LPage.Add(LGroup);
  LGroup.Add(NewNyxLabel('grid-first').WithText('First').Configure.Height(28).Done);
  LGroup.Add(NewNyxLabel('grid-second').WithText('Second').Configure.Height(28).Done);
  LGroup := NewNyxGroup('fixed-settings');
  LGroup.WithText('Working notes').Configure.Layout(nlColumn).Height(180).Padding(8).Done;
  LPage.Add(LGroup);
  LGroup.Add(NewNyxMemo('fill-notes').WithText('Notes')
    .Configure.Flex(1).HeightSizing(nsFill).Done);
  LGroup := NewNyxGroup('empty-settings');
  LGroup.WithText('Reserved options').Configure.Layout(nlColumn).Padding(8).Done;
  LPage.Add(LGroup);

  { A distant ordinary group forces the runtime's logical viewport path. It
    starts parked, so subsequent measurement must retain its native decorations
    despite a real zero-area allocation. The group itself stays small enough to
    reveal completely; oversized native group painting remains explicitly refused. }
  LPage.Add(NewNyxSpacer('long-content').Configure.Height(70000).Done);
  LGroup := NewNyxGroup('distant-settings');
  LGroup.WithText('Later settings').Configure.Layout(nlColumn).Padding(8).Gap(8).Done;
  LPage.Add(LGroup);
  LGroup.Add(NewNyxMemo('distant-notes').WithText('Notes').Configure.Height(120).Done);
  LNested := NewNyxGroup('distant-preferences');
  LNested.WithText('More preferences').Configure.Layout(nlColumn).Padding(8).Done;
  LGroup.Add(LNested);
  LNested.Add(NewNyxMemo('distant-details').WithText('Details').Configure.Height(100).Done);
end;

procedure CheckContent(const AGroupID, AChildID: TNyxText; APadding: Integer);
{$ifdef PAS2JS}
var
  LGroup: TJSDOMRect;
  LChild: TJSDOMRect;
begin
  LGroup := Face(AGroupID).getBoundingClientRect;
  LChild := Face(AChildID).getBoundingClientRect;
  Check((LChild.left >= LGroup.left + APadding) and
    (LChild.right <= LGroup.right - APadding + 1),
    'Child stays inside group content width: ' + AChildID);
  Check(LChild.bottom <= LGroup.bottom - APadding + 1,
    'Natural group height includes caption/frame and child content: ' + AChildID);
end;
{$else}
var
  LGroup: TGroupBox;
  LChild: TControl;
begin
  LGroup := TGroupBox(Face(AGroupID));
  LChild := Face(AChildID);
  WriteLn('GROUP ', AGroupID, ' outer=', LGroup.Width, ',', LGroup.Height,
    ' client=', LGroup.ClientWidth, ',', LGroup.ClientHeight,
    ' child=', LChild.Left, ',', LChild.Top, ',', LChild.Width, ',', LChild.Height);
  Check((LChild.Left >= APadding) and
    (LChild.Left + LChild.Width <= LGroup.ClientWidth - APadding),
    'Child stays inside actual native group client width: ' + AChildID);
  Check(LChild.Top + LChild.Height <= LGroup.ClientHeight - APadding,
    'Natural native group height includes caption/frame and child content: ' + AChildID);
end;
{$endif}

procedure Run(ADesignMode: Boolean);
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LGroup: TFace;
  LNested: TFace;
  LDetails: TFace;
  LLongGroup: TFace;
  {$ifdef PAS2JS}
  LMemo: TJSHTMLTextAreaElement;
  LStyle: TJSHTMLElement;
  LTheme: TNyxTheme;
  {$else}
  LMemo: TMemo;
  LPosition: TPoint;
  LNativeBounds: TRect;
  {$endif}
begin
  LDocument := Fixture;
  GRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '640px');
  GHost.style.setProperty('height', '540px');
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
  GHost.ClientHeight := 540;
  GHost.Show;
  {$endif}
  try
    GRenderer.Render(LDocument, LDocument.Pages[0], GHost, ADesignMode);
    Pump;
    LSource := TNyxCodegen.Generate(LDocument);
    LGroup := Face('settings');
    LNested := Face('preferences');
    CheckContent('settings', 'notes', 12);
    CheckContent('settings', 'updates', 12);
    CheckContent('settings', 'preferences', 12);
    CheckContent('preferences', 'details', 8);
    {$ifndef PAS2JS}

    if not ADesignMode then
    begin
      CheckPointerMapping('settings');
    end;
    {$endif}
    {$ifdef PAS2JS}
    LMemo := TJSHTMLTextAreaElement(Face('notes').querySelector('textarea'));
    LMemo.focus;
    LMemo.value := 'Notes for tomorrow';
    LMemo.selectionStart := 5;
    LMemo.selectionEnd := 5;
    GHost.style.setProperty('width', '390px');
    {$else}
    LMemo := TMemo(GRenderer.InputFor('notes'));
    LMemo.SetFocus;
    LMemo.Text := 'Notes for tomorrow';
    LMemo.SelStart := 5;
    LMemo.SelLength := 0;
    GHost.ClientWidth := 390;
    {$endif}
    Pump;
    CheckContent('settings', 'notes', 12);
    CheckContent('settings', 'updates', 12);
    CheckContent('settings', 'preferences', 12);
    CheckContent('preferences', 'details', 8);
    Check((Face('settings') = LGroup) and (Face('preferences') = LNested),
      'Narrow resize retains both independent group controls');
    {$ifdef PAS2JS}
    Check((TJSHTMLTextAreaElement(Face('notes').querySelector('textarea')) = LMemo) and
      (LMemo.value = 'Notes for tomorrow') and (LMemo.selectionStart = 5),
      'Narrow browser group retains its memo draft and caret');
    {$else}
    Check((GRenderer.InputFor('notes') = LMemo) and LMemo.Focused and
      (LMemo.Text = 'Notes for tomorrow') and (LMemo.SelStart = 5),
      'Narrow native group retains its memo focus, draft and caret');

    { Native frame metrics must survive real caption/font changes as well as a
      parked zero-area window. This is a font-size simulation, not hardware DPI. }
    TGroupBox(Face('settings')).Font.Size := 18;
    GRenderer.Sync;
    Pump;
    CheckContent('settings', 'notes', 12);
    CheckContent('settings', 'preferences', 12);
    CheckContent('preferences', 'details', 8);
    {$endif}
    LDetails := Face('details');
    {$ifdef PAS2JS}
    GHost.style.setProperty('height', '160px');
    GHost.style.setProperty('overflow', 'auto');
    {$else}GHost.ClientHeight := 160;{$endif}
    Pump;
    GRenderer.Reveal('details');
    Pump;
    {$ifndef PAS2JS}
    GRenderer.FocusFor('details').SetFocus;
    Pump;
    LPosition := Face('home').Parent.ScreenToClient(
      Face('details').ClientToScreen(Point(0, 0)));
    Check((LPosition.Y >= 0) and
      (LPosition.Y + Face('details').Height <= GRenderer.ViewViewport.Height),
      'Actual revealed nested group input fits its containing viewport');
    Check(GRenderer.FocusFor('details').Focused, 'Revealed nested memo accepts actual native focus');
    {$endif}
    Check(Face('details') = LDetails, 'Scrolling retains the original nested group field');
    GRenderer.ScrollView(0, 0);
    GRenderer.Reveal('details');
    Pump;
    Check(Face('details') = LDetails, 'Repeated group reveal retains the same field');
    {$ifdef PAS2JS}GHost.style.setProperty('height', '540px');
    {$else}GHost.ClientHeight := 540;{$endif}
    Pump;
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      SaveNyxNativeCapture(GHost, IncludeTrailingPathDelimiter(ParamStr(1)) +
        'group-' + BoolToStr(ADesignMode, True) + '-390.png', ncmPrint);
    end;
    {$endif}
    Check(TNyxCodegen.Generate(LDocument) = LSource,
      'Group geometry and local input leave authored Pascal unchanged');
    GRenderer.Render(LDocument, LDocument.Pages[1], GHost, ADesignMode);
    Pump;
    CheckContent('row-settings', 'row-first', 8);
    CheckContent('row-settings', 'row-second', 8);
    CheckContent('grid-settings', 'grid-first', 8);
    CheckContent('grid-settings', 'grid-second', 8);
    CheckContent('fixed-settings', 'fill-notes', 8);
    {$ifndef PAS2JS}
    Check(Face('fill-notes').Height = TGroupBox(Face('fixed-settings')).ClientHeight - 16,
      'Fixed-height group assigns its exact usable main-axis space to the flexible memo');
    Check(TGroupBox(Face('empty-settings')).ClientHeight = 16,
      'Empty native group retains its authored padding below the caption');
    {$endif}

    LLongGroup := Face('distant-settings');
    {$ifndef PAS2JS}
    Check((LLongGroup.Width = 0) and (LLongGroup.Height = 0),
      'Distant native group begins parked with no physical area');
    Check((LCLIntf.GetWindowRect(TWinControl(LLongGroup).Handle, LNativeBounds) <> 0) and
      (LNativeBounds.Right = LNativeBounds.Left) and
      (LNativeBounds.Bottom = LNativeBounds.Top),
      'Actual owned parked group window has zero area, independently of cached bounds');
    {$endif}
    GRenderer.Reveal('distant-settings');
    Pump;
    CheckContent('distant-settings', 'distant-notes', 8);
    CheckContent('distant-settings', 'distant-preferences', 8);
    CheckContent('distant-preferences', 'distant-details', 8);
    {$ifndef PAS2JS}

    if not ADesignMode then
    begin
      CheckPointerMapping('distant-settings');
    end;
    LMemo := TMemo(GRenderer.InputFor('distant-notes'));
    LMemo.SetFocus;
    LMemo.Text := 'A retained distant draft';
    LMemo.SelStart := 4;
    GRenderer.FocusFor('fill-notes').SetFocus;
    GRenderer.ScrollView(0, 0);
    Pump;
    Check((LLongGroup.Width = 0) and (LLongGroup.Height = 0),
      'Previously realized distant group parks again without retirement');
    Check((LCLIntf.GetWindowRect(TWinControl(LLongGroup).Handle, LNativeBounds) <> 0) and
      (LNativeBounds.Right = LNativeBounds.Left) and
      (LNativeBounds.Bottom = LNativeBounds.Top),
      'Actual reparked native group window retains its zero-area allocation');
    { Sync performs another natural measurement while both group HWNDs are
      parked. Revealing them again must restore the same usable client geometry. }
    GRenderer.Sync;
    GRenderer.Reveal('distant-settings');
    Pump;
    CheckContent('distant-settings', 'distant-notes', 8);
    CheckContent('distant-settings', 'distant-preferences', 8);
    CheckContent('distant-preferences', 'distant-details', 8);
    Check((Face('distant-settings') = LLongGroup) and
      (GRenderer.InputFor('distant-notes') = LMemo) and
      (LMemo.Text = 'A retained distant draft') and (LMemo.SelStart = 4),
      'Distant group remeasurement retains its owned control, memo draft and caret');
    Check(TNyxCodegen.Generate(LDocument) = LSource,
      'Logical group parking and local input leave authored Pascal unchanged');
    {$endif}
    GRenderer.Unmount;
    Pump;
  finally
    GRenderer.Free;
    GRenderer := nil;
    {$ifdef PAS2JS}GHost.remove;
    LStyle.remove;{$else}GHost.Free;{$endif}
    LDocument.Free;
  end;
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Run(False);
    Run(True);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-group-controls', 'passed');
    document.body.setAttribute('data-group-checks', IntToStr(GChecks));
    {$else}WriteLn('PASS ', GChecks, ' actual group content/control checks');{$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-group-controls', 'failed');
      document.body.setAttribute('data-group-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
