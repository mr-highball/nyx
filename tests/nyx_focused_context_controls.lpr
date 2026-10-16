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
program nyx_focused_context_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.state, nyx.codec, nyx.composition, nyx.canvas.types, nyx.application.view, nyx.test.focused.context
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl{$endif};

{$ifndef PAS2JS}
type
  { Fixture-owned receiver for deferred LCL failures. It borrows the exception
    only during dispatch, copying its message instead of opening a modal dialog.
    The Application callback is cleared before this receiver is destroyed. }
  TFailure = class
  public
    MessageText: String;
    procedure Failed(Sender: TObject; E: Exception);
  end;
{$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GFailure: TFailure;
  {$endif}

{$ifndef PAS2JS}
procedure TFailure.Failed(Sender: TObject; E: Exception);
begin
  MessageText := E.Message;
end;
{$endif}

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
{ Borrow the browser timer for two actual event-loop turns. It retains only the
  promise resolver until dispatch and no application or authored document. }
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure Idle; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  {$else}
  for LTurn := 1 to 3 do
  begin
    Application.ProcessMessages;
    CheckSynchronize;

    if GFailure.MessageText <> '' then
    begin
      raise Exception.Create(GFailure.MessageText);
    end;
    Sleep(10);
  end;
  {$endif}
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
const
  CViews: array[0..5] of TNyxText = ('home', 'Wrapper', 'focused-memo',
    'nested-canvas', FocusedUnicodeID, 'Wrapper');
var
  LDocument: TNyxDocument;
  LFocused: TNyxDocument;
  LComponentOnly: TNyxDocument;
  LBefore: TNyxText;
  LIndex: Integer;
  LFailed: Boolean;
  {$ifdef PAS2JS}
  LApp: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  {$else}
  LApp: TNyxLCLApplication;
  LMemo: TCustomMemo;
  {$endif}
begin
  LDocument := FocusedContextDocument;
  LFocused := nil;
  LComponentOnly := nil;
  LApp := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$endif}
  try
    LBefore := TNyxCodec.Encode(LDocument);
    for LIndex := Low(CViews) to High(CViews) do
    begin
      if LIndex = High(CViews) then
      begin
        LComponentOnly := LDocument.Clone;
        LComponentOnly.Canvases.Remove(NyxCanvasRef('Page library'));
        LFocused := CloneNyxViewDocument(LComponentOnly, LComponentOnly.Find('Wrapper'));
        FreeAndNil(LComponentOnly);
        Check(LFocused.Count = 0, 'a reusable-only context needs no invented authored page');
      end
      else
      begin
        LFocused := CloneNyxViewDocument(LDocument, LDocument.Find(CViews[LIndex]));
      end;
      {$ifdef PAS2JS}
      LApp := TNyxBrowserApplication.Create;
      LApp.ConfigureView(TNyxApplicationView.Focused(NyxControl(CViews[LIndex])));
      LApp.Run(LFocused, LHost);
      await(Idle);
      {$else}
      LApp := TNyxLCLApplication.Create;
      LApp.ConfigureView(TNyxApplicationView.Focused(NyxControl(CViews[LIndex])));
      LApp.Mount(LFocused);

      if ParamStr(1) = '--compact' then
      begin
        LApp.Window.SetBounds(15, 15, 390, 760);
      end;
      LApp.Window.Show;
      Idle;
      {$endif}
      Check((LApp.View.Root.ID = CViews[LIndex]) and (LApp.NavigationState = nil),
        'actual target renders only the exact focused control, without context-page navigation');

      if LIndex in [1, 2, 5] then
      begin
        {$ifdef PAS2JS}
        LMemo := TJSHTMLTextAreaElement(LApp.View.InputFor('focused-memo'));
        Check(LMemo.value = 'Write a focused reply', 'focused memo receives independent typed runtime defaults');
        LMemo.value := 'Edited focused reply';
        LMemo.dispatchEvent(TJSEvent.new('input'));
        await(Idle);
        {$else}
        LMemo := TCustomMemo(LApp.View.InputFor('focused-memo'));
        Check(LMemo.Text = 'Write a focused reply', 'focused memo receives independent typed runtime defaults');
        LMemo.Text := 'Edited focused reply';
        Idle;
        {$endif}
        Check(LApp.State.GetValue(NyxTextState('reply')) = 'Edited focused reply',
          'actual focused input writes the ordinary application binding');

        if LIndex = 2 then
        begin
          Check(LApp.View.Root.ID <> LFocused.Pages[0].ID,
            'a retained context page never becomes the displayed descendant');
        end;
      end;

      if LIndex = 4 then
      begin
        {$ifdef PAS2JS}
        Check(LApp.View.ElementFor(FocusedUnicodeID).textContent = FocusedUnicodeCaption,
          'the actual focused control preserves supplementary caption and source identity');
        {$else}
        Check(TCustomLabel(LApp.View.ControlFor(FocusedUnicodeID)).Caption = TNyxText(FocusedUnicodeCaption),
          'the actual focused control preserves supplementary caption and source identity');
        {$endif}
      end;
      LFailed := False;
      try
        LApp.ConfigureView(Default(TNyxApplicationView));
      except
        on ENyxModel do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and (LApp.View.Root.ID = CViews[LIndex]),
        'post-mount focus reconfiguration refuses without altering the accepted view');
      FreeAndNil(LApp);
      {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
      FreeAndNil(LFocused);
    end;
    {$ifdef PAS2JS}
    LApp := TNyxBrowserApplication.Create;
    LApp.Run(LDocument, LHost);
    await(Idle);
    {$else}
    LApp := TNyxLCLApplication.Create;
    LApp.Mount(LDocument);
    LApp.Window.Show;
    Idle;
    {$endif}
    Check((LApp.View.Root.ID = 'home') and (LApp.NavigationState <> nil),
      'default application still displays the first page with ordinary navigation');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'focused and normal actual controls never mutate the borrowed authored source');
  finally
    LApp.Free;
    LFocused.Free;
    LComponentOnly.Free;
    LDocument.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
    document.body.setAttribute('data-focused-controls', 'passed');
    document.body.setAttribute('data-focused-controls-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-focused-controls', 'failed');
      document.body.setAttribute('data-focused-controls-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  window.setTimeout(@Start, 50);
  {$else}
  try
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    try
      Run;
    finally
      Application.OnException := nil;
      FreeAndNil(GFailure);
    end;
    WriteLn('PASS ', GChecks, ' actual focused application checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
