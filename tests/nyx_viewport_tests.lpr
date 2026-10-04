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
program nyx_viewport_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.viewport, nyx.model, nyx.behavior,
  nyx.events, nyx.callbacks, nyx.scheduler, nyx.schema, nyx.codegen,
  nyx.studio.session, nyx.studio.inspector,
  {$ifdef NYX_COMPILED_VIEWPORT}nyx.viewport.fixture,{$endif}
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, Grids, ComCtrls,
  Types, LCLIntf, LCLType, LMessages, nyx.render.lcl;{$endif}

type
  { The probe owns snapshots, never the control or document. Its renderer field
    is borrowed only for the synchronous navigation case, then cleared. }
  TProbe = class(TNyxEventCallback, INyxCallbackFactory)
    Counts: array[TNyxTrigger] of Integer;
    Last: array[TNyxTrigger] of TNyxEventInfo;
    Order: TNyxText;
    Consume: TNyxTrigger;
    Navigate: TNyxTrigger;
    DestroyViewport: Boolean;
    CanConsume: array[TNyxTrigger] of Boolean;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;{$else}Renderer: TNyxLCLRenderer;{$endif}
    procedure Reset;
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TWheel = class external name 'WheelEvent' (TJSWheelEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TAccess = class(TWinControl);
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Viewport: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TProbe.Reset;
var
  LTrigger: TNyxTrigger;
begin
  Order := '';
  Consume := ntDesignSelect;
  Navigate := ntDesignSelect;
  DestroyViewport := False;
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin
    Counts[LTrigger] := 0;
    CanConsume[LTrigger] := False;
  end;
end;

function TProbe.Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
begin
  Result := Self;
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Counts[AEvent.Trigger]);
  Last[AEvent.Trigger] := AEvent.Copy;
  Order := Order + NyxTriggerName(AEvent.Trigger) + '|';
  CanConsume[AEvent.Trigger] := NyxEventResponse(AExecution).CanConsume;

  if DestroyViewport and (AEvent.Trigger = ntScroll) then
  begin
    Renderer.Free;
    Renderer := nil;
    Exit;
  end;

  if AEvent.Trigger = Consume then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if AEvent.Trigger = Navigate then
  begin
    Renderer.Unmount;
  end;
end;

function Fixture: TNyxDocument;
var
  LPage, LScroll, LNode: TNyxNode;
  LIndex: Integer;
  LLines, LRows: TNyxText;
begin
  LLines := '';
  LRows := 'Name' + #9 + 'Value';
  for LIndex := 1 to 80 do
  begin
    LLines := LLines + TNyxText('Line ') + IntToStr(LIndex) + TNyxText(' / 🌙 漢字') + #10;
    LRows := LRows + #10 + 'Row ' + IntToStr(LIndex) + #9 + IntToStr(LIndex);
  end;
  Result := TNyxDocument.Create;
  LPage := TNyxNode.Create(nkPage, 'home');
  Result.AddPage(LPage);
  LPage.Configure.Layout(nlColumn).Gap(12).Padding(12).Done;
  LScroll := TNyxNode.Create(nkScroll, 'content-scroll');
  LScroll.Configure.Height(140).Width(500).Done;
  LPage.Add(LScroll);
  for LIndex := 1 to 20 do
  begin
    LScroll.Add(TNyxNode.Create(nkLabel, 'line-' + IntToStr(LIndex))
      .Configure.Text('Scrollable content ' + IntToStr(LIndex)).Height(32).Done);
  end;
  LNode := TNyxNode.Create(nkMemo, 'reply-memo').Configure.Text('Reply')
    .Value(LLines).Height(140).Done;
  LPage.Add(LNode);
  NyxCallbacks(LNode).OnBeforeWheel.Add(NyxHandler('BeforeWheel'), NyxCallbackID('before-wheel'));
  NyxCallbacks(LNode).OnWheel.Add(NyxHandler('Wheel'), NyxCallbackID('wheel'));
  NyxCallbacks(LNode).OnAfterWheel.Add(NyxHandler('AfterWheel'), NyxCallbackID('after-wheel'));
  NyxCallbacks(LNode).OnScroll.Add(NyxHandler('Scroll'), NyxCallbackID('scroll'));
  LPage.Add(TNyxNode.Create(nkCodeEditor, 'source-editor').Configure.Value(LLines)
    .Height(140).Done);
  LPage.Add(TNyxNode.Create(nkCode, 'source-block').Configure.Text(LLines)
    .Height(140).Done);
  LPage.Add(TNyxNode.Create(nkList, 'result-list').Configure.Items(LLines)
    .Height(140).Done);
  LPage.Add(TNyxNode.Create(nkTable, 'result-table').Configure.Items(LRows)
    .Height(140).Done);
  LPage.Add(TNyxNode.Create(nkTree, 'result-tree').Configure.Items(LLines)
    .Height(140).Done);
end;

procedure ContractChecks;
var
  LWheel: TNyxWheelSnapshot;
  LAxis: TNyxViewportAxis;
  LView: TNyxViewportSnapshot;
  LBad: Boolean;
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LIndex, LCount, LLine: Integer;
  LSession: TNyxStudioSession;
  LHandler: TNyxHandlerRef;
begin
  LWheel := Default(TNyxWheelSnapshot);
  LAxis := Default(TNyxViewportAxis);
  LView := Default(TNyxViewportSnapshot);
  Check(not LWheel.Defined and not LAxis.Defined and not LView.Defined,
    'default records are absent observations');
  LWheel := NyxWheel(-1.25, 2.5, 0.125, nwuLines, [nmControl], False);
  Check((LWheel.X = -1.25) and (LWheel.Y = 2.5) and (LWheel.Z = 0.125) and
    (LWheel.Units = nwuLines) and not LWheel.CanCancel, 'exact units/fractions/cancellation');
  LAxis := NyxViewportAxis(-12.5, 800, 140, nvuLogicalPixels);
  LView := NyxViewport(LAxis, LAxis, 120, 140);
  Check(LView.SamePosition(LView) and (LView.X.Position = -12.5), 'signed local offsets');
  LBad := False;
  try
    LAxis := NyxViewportAxis(0, -1, 10, nvuLogicalPixels);
  except on EArgumentException do LBad := True; end;
  Check(LBad, 'negative extents refused');
  LBad := False;
  try
    LWheel := NyxWheel(NaN, 0, 0, nwuPixels, [], True);
  except on EArgumentException do LBad := True; end;
  Check(LBad, 'nonfinite deltas refused');
  LDocument := Fixture;
  try
    LMetadata := NyxEventsMetadata(LDocument.Pages[0].Find('reply-memo'), LDocument);
    LCount := 0;
    for LIndex := 0 to High(LMetadata) do
    begin

      if LMetadata[LIndex].Trigger in [ntBeforeWheel, ntWheel, ntAfterWheel, ntScroll, ntScrollEnd] then
      begin
        Inc(LCount);

        if LMetadata[LIndex].Trigger = ntScroll then
        begin
          Check((LMetadata[LIndex].Browser = ncAvailable) and
            (LMetadata[LIndex].Native = ncBasic), 'coalesced native support explicit');
        end;

        if LMetadata[LIndex].Trigger = ntScrollEnd then
        begin
          Check((LMetadata[LIndex].Browser = ncBasic) and
            (LMetadata[LIndex].Native = ncMissing), 'unsupported gesture completion explicit');
        end;
      end;
    end;
    Check(LCount = 5, 'scrollable editor publishes the complete wheel/viewport family');
  finally
    LDocument.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Select('project-name');
    LHandler := LSession.AddCallback(ntBeforeWheel, LLine);
    Check(Pos('TODO', LSession.Source) > 0, 'Studio creates authored TODO handler');
    Check(Pos(LHandler.Name, LSession.Source) > 0, 'Studio source names handler');
    Check(LSession.CanUndo, 'callback edit undoable');
    LSession.Undo;
    Check(LSession.CanRedo, 'callback edit replayable');
    LSession.Redo;
    Check((LSession.CallbackLine(LHandler) > 0) and (LLine > 0), 'callback source navigation');
  finally
    LSession.Free;
  end;
end;

procedure RunControls;
var
  LDocument: TNyxDocument;
  LProbe: TProbe;
  LFactory: INyxCallbackFactory;
  LCallback: INyxEventCallback;
  LBefore: TNyxViewportSnapshot;
  LSourceBefore: TNyxText;
  LIndex: Integer;
  LNames: array of TNyxText;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost, LInput, LControl: TJSHTMLElement;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TMemo;
  {$endif}

  function Wheel(ACancelable: Boolean = True; AHorizontal: Boolean = False): Boolean;
  {$ifdef PAS2JS}
  var
    LOptions: TJSObject;
    LEvent: TWheel;
  begin
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := ACancelable;
    LOptions['deltaX'] := 0.5;
    LOptions['deltaY'] := 1.25;
    LOptions['deltaZ'] := 0.125;
    LOptions['deltaMode'] := 1;
    LOptions['ctrlKey'] := True;
    LEvent := TWheel.new('wheel', LOptions);
    LInput.dispatchEvent(LEvent);
    Result := LEvent.defaultPrevented;
  end;
  {$else}
  begin

    if AHorizontal then
    begin
      Result := TAccess(LInput).DoMouseWheelHorz([ssCtrl], 60, Point(12, 8));
    end
    else
    begin
      Result := TAccess(LInput).DoMouseWheel([ssCtrl], -150, Point(12, 8));
    end;
  end;
  {$endif}

begin
  {$ifdef NYX_COMPILED_VIEWPORT}
  LDocument := nyx.viewport.fixture.BuildNyxDocument;
  {$else}
  LDocument := Fixture;
  {$endif}
  LProbe := TProbe.Create;
  LFactory := LProbe;
  LCallback := LProbe;
  LProbe.Reset;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 640, 720);
  LHost.Show;
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    LProbe.Renderer := LRenderer;
    {$ifdef PAS2JS}
    LInput := TJSHTMLElement(LRenderer.ElementFor('reply-memo').querySelector('textarea'));
    {$else}
    LInput := TMemo(LRenderer.InputFor('reply-memo'));
    LInput.HandleNeeded;
    {$endif}
    LBefore := LRenderer.ViewportFor('reply-memo');
    LSourceBefore := TNyxCodegen.Generate(LDocument);
    Wheel;
    Check(LProbe.Order = 'before-wheel|wheel|after-wheel|', 'ordered independent wheel phases');
    Check(LProbe.Last[ntWheel].HasWheel and (LProbe.Last[ntWheel].Wheel.Y = 1.25),
      'real widget exact signed delta');
    Check(nmControl in LProbe.Last[ntWheel].Wheel.Modifiers, 'wheel shortcut modifiers');
    Check(not LProbe.CanConsume[ntAfterWheel] and LProbe.CanConsume[ntBeforeWheel],
      'only synchronous before/main callbacks consume');
    Check(LRenderer.ViewportFor('reply-memo').SamePosition(LBefore),
      'wheel callback dispatch does not fabricate actual movement');
    LProbe.Reset;
    LProbe.Consume := ntBeforeWheel;
    Check(Wheel, 'sequential before callback cancels actual widget request');
    Check((LProbe.Counts[ntWheel] = 0) and (LProbe.Counts[ntAfterWheel] = 1) and
      LProbe.Last[ntAfterWheel].DefaultPrevented, 'consumption skips main, after observes decision');
    {$ifdef PAS2JS}
    LProbe.Reset;
    Wheel(False);
    Check(not LProbe.CanConsume[ntBeforeWheel] and not LProbe.CanConsume[ntWheel],
      'uncancelable physical input has read-only responses');
    Check(not LProbe.Last[ntAfterWheel].DefaultPrevented, 'uncancelable request is not claimed prevented');
    {$else}
    LProbe.Reset;
    Wheel(True, True);
    Check((LProbe.Last[ntWheel].Wheel.X = 0.5) and (LProbe.Last[ntWheel].Wheel.Y = 0) and
      (LProbe.Last[ntWheel].Wheel.Units = nwuDetents), 'horizontal native detents point right');
    {$endif}
    LProbe.Reset;
    LNames := ['content-scroll', 'reply-memo', 'source-editor', 'source-block',
      'result-list', 'result-table', 'result-tree'];
    for LIndex := 0 to High(LNames) do
    begin
      LRenderer.Events.OnScroll(NyxControlEvents(LNames[LIndex])).Subscribe(LCallback);
      LBefore := LRenderer.ViewportFor(LNames[LIndex]);
      {$ifdef PAS2JS}
      LControl := LRenderer.ElementFor(LNames[LIndex]);

      if (LNames[LIndex] = 'reply-memo') then
      begin
        LControl := TJSHTMLElement(LControl.querySelector('textarea'));
      end;
      LControl.scrollTop := 80;
      LControl.dispatchEvent(TJSEvent.new('scroll'));
      {$else}
      case LIndex of
        0: TScrollBox(LRenderer.ControlFor(LNames[LIndex])).VertScrollBar.Position := 80;
        1, 2, 3:
          begin
            LInput := TMemo(LRenderer.ControlFor(LNames[LIndex]));

            if LIndex = 1 then
            begin
              LInput := TMemo(LRenderer.InputFor(LNames[LIndex]));
            end;
            LInput.SetFocus;
            { Exercise the real native scrollbar command. Moving the caret
              alone is not a portable promise to move a memo's viewport. }
            SendMessage(LInput.Handle, LM_VSCROLL, SB_PAGEDOWN, 0);
          end;
        4: TListBox(LRenderer.ControlFor(LNames[LIndex])).TopIndex := 20;
        5: TStringGrid(LRenderer.ControlFor(LNames[LIndex])).TopRow := 20;
        6: TTreeView(LRenderer.ControlFor(LNames[LIndex])).TopItem :=
          TTreeView(LRenderer.ControlFor(LNames[LIndex])).Items[20];
      end;
      Application.Idle(False);
      {$endif}
      Check(not LRenderer.ViewportFor(LNames[LIndex]).SamePosition(LBefore),
        'actual scroll position changed: ' + LNames[LIndex]);
      Check(LProbe.Last[ntScroll].HasViewport and
        (LProbe.Last[ntScroll].OriginID = LNames[LIndex]), 'owned actual observation: ' + LNames[LIndex]);
      Check(not LProbe.CanConsume[ntScroll], 'scroll cannot consume past movement: ' + LNames[LIndex]);
    end;
    Check(TNyxCodegen.Generate(LDocument) = LSourceBefore, 'scrolling leaves design/source unchanged');
    LBefore := LProbe.Last[ntScroll].Viewport;
    LProbe.Reset;
    LProbe.Navigate := ntBeforeWheel;
    {$ifdef PAS2JS}
    LInput := TJSHTMLElement(LRenderer.ElementFor('reply-memo').querySelector('textarea'));
    {$else}
    LInput := TMemo(LRenderer.InputFor('reply-memo'));
    {$endif}
    Wheel;
    Check((LProbe.Counts[ntBeforeWheel] = 1) and (LProbe.Counts[ntAfterWheel] = 0),
      'wheel navigation ends remaining phases safely');
    {$ifndef PAS2JS}Application.Idle(False);{$endif}
    Check(LBefore.Defined, 'retained viewport survives disposal');
    {$ifndef PAS2JS}
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LProbe.Reset;
    LProbe.DestroyViewport := True;
    TScrollBox(LRenderer.ControlFor('content-scroll')).VertScrollBar.Position := 80;
    Application.Idle(False);
    Check((LProbe.Renderer = nil) and LProbe.Last[ntScroll].HasViewport,
      'native idle callback can destroy its renderer without a stale producer');
    LRenderer := nil;
    Application.Idle(False);
    {$endif}
    LProbe.Renderer := nil;
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LCallback := nil;
    LFactory := nil;
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
procedure RunBrowserMovement;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxBrowserRenderer;
  LHost, LScroll: TJSHTMLElement;
  LProbe: TProbe;
  LCallback: INyxEventCallback;
  LAttempts: Integer;

  procedure Finish;
  var
    LOldScroll: TJSHTMLElement;
    LCalls: Integer;
  begin
    try
      Check((LProbe.Counts[ntScroll] > 0) and LProbe.Last[ntScroll].HasViewport,
        'browser itself emits actual scroll after programmatic movement');
      Check((LProbe.Counts[ntScrollEnd] > 0) and not LProbe.CanConsume[ntScrollEnd],
        'browser itself emits actual completion with read-only response');
      Check(LProbe.Last[ntScrollEnd].Viewport.Y.Position = LScroll.scrollTop,
        'completion owns final actual offset');
      LOldScroll := LScroll;
      LCalls := LProbe.Counts[ntScroll];
      LRenderer.Unmount;
      LOldScroll.dispatchEvent(TJSEvent.new('scroll'));
      LOldScroll.dispatchEvent(TJSEvent.new('scrollend'));
      Check(LProbe.Counts[ntScroll] = LCalls, 'detached DOM producer listeners revoked');
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' viewport checks';
      document.body.setAttribute('data-viewport-tests', 'passed');
    except on LException: Exception do
      begin
        document.body.textContent := 'FAIL ' + LException.Message;
        document.body.setAttribute('data-viewport-tests', 'failed');
      end;
    end;
    LRenderer.Free;
    LHost.remove;
    LCallback := nil;
    LDocument.Free;
  end;

  procedure Observe;
  begin
    Inc(LAttempts);

    if ((LProbe.Counts[ntScroll] > 0) and (LProbe.Counts[ntScrollEnd] > 0)) or
      (LAttempts >= 30) then
    begin
      Finish;
    end
    else
    begin
      window.setTimeout(@Observe, 100);
    end;
  end;

begin
  LDocument := Fixture;
  LRenderer := TNyxBrowserRenderer.Create;
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
  LProbe := TProbe.Create;
  LCallback := LProbe;
  LProbe.Reset;
  LRenderer.Events.OnScroll(NyxControlEvents('content-scroll')).Subscribe(LCallback);
  LRenderer.Events.OnScrollEnd(NyxControlEvents('content-scroll')).Subscribe(LCallback);
  LScroll := LRenderer.ElementFor('content-scroll');
  LScroll.scrollTop := 100;
  LAttempts := 0;
  { This bounded test waits for genuine host notifications; it does not dispatch
    a synthetic scroll event or infer gesture completion from an idle timeout. }
  window.setTimeout(@Observe, 100);
end;
{$else}
procedure WriteFixture;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LFile: TFileStream;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := Fixture;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.viewport.fixture');
    LFile := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LFile.WriteBuffer(LSource[1], Length(LSource));
    finally
      LFile.Free;
    end;
  finally
    LDocument.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    ContractChecks;
    RunControls;
    {$ifdef PAS2JS}
    RunBrowserMovement;
    {$else}
    WriteFixture;
    WriteLn('PASS ', GChecks, ' viewport checks');
    {$endif}
  except on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-viewport-tests', 'failed');
      {$else}WriteLn('FAIL ', LException.Message); ExitCode := 1;{$endif}
    end
    {$ifdef PAS2JS}
    else
    begin
      { Preserve non-Pascal host failures instead of producing an empty capture. }
      document.body.textContent := 'FAIL browser host: ' +
        String(TJSObject(JSExceptValue)['stack']);
      document.body.setAttribute('data-viewport-tests', 'failed');
    end
    {$endif};
  end;
end.
