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
program nyx_binding_container_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.codec, nyx.presentations, nyx.containers, nyx.responsive,
  {$ifdef PAS2JS}JS, Web, nyx.application.browser;
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  nyx.application.lcl;{$endif}

const
  CLong: TNyxText = 'The status now explains the work in progress. This longer ' +
    'description wraps inside its own narrow column, leaving less room for the ' +
    'tools below. Their presentation should follow the space they actually ' +
    'receive, while your unfinished notes stay exactly where you left them.';
  CDraft: TNyxText = 'A working draft 🌙';
  CThreshold = 240;
  CInset = 8;

var
  GDocument: TNyxDocument;
  GOriginal: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GAreaHeight: Double;
  GLabelHeight: Double;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GArea: TJSHTMLElement;
  GStatus: TJSHTMLElement;
  GButton: TJSHTMLButtonElement;
  GNotes: TJSHTMLTextAreaElement;
  GCapture: Boolean;
  GContinue: TJSHTMLButtonElement;
  GFinish: TJSHTMLButtonElement;
  {$else}
  GApp: TNyxLCLApplication;
  GArea: TWinControl;
  GStatus: TLabel;
  GButton: TNyxLCLButton;
  GNotes: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Container state controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LStatus: INyxLabel;
  LArea: INyxCard;
  LButton: INyxButton;
  LNotes: INyxMemo;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Space that follows your work';
    Result.State.SetValue(NyxTextState('status'), 'Ready')
      .SetValue(NyxTextState('notes'), 'Notes');
    Result.Presentations.Define(NyxPresentation('tight tools'),
      TNyxPresentationCondition.Within(NyxContainer('available space'),
        TNyxViewportCondition.Any.HeightBelow(CThreshold)));
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Height(480).MinimumHeight(0)
      .Padding(CInset).Gap(8).Done;
    Result.AddPage(LPage);
    LStatus := NewNyxLabel('status');
    LStatus.Configure.Width(200).Done;
    LStatus.Binds.Text(NyxTextState('status')).Done;
    LPage.Add(LStatus);
    LArea := NewNyxCard('available');
    LArea.Configure.Layout(nlColumn).Flex(1).HeightSizing(nsFill).MinimumHeight(0)
      .Padding(CInset).Gap(8).QueryContainer(NyxContainer('available space'))
      .Containment(nccSize).Done;
    LPage.Add(LArea);
    LButton := NewNyxButton('detail-action').WithText('Open details');
    LButton.Configure.WhenPresentation(NyxPresentation('tight tools'))
      .Text('More room needed').Enabled(False).Done;
    LArea.Add(LButton);
    LNotes := NewNyxMemo('notes');
    LNotes.Configure.Text('Working notes').Height(100).Done;
    LNotes.Binds.Value(NyxTextState('notes')).Done;
    LPage.Add(LNotes);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

{ Read the actual allocated client box, removing the authored padding. Neither
  branch estimates the publisher from its model or a viewport-size surrogate. }
function AreaHeight: Double;
begin
  Result := Max(0, GArea.ClientHeight - CInset * 2);
end;

function LabelHeight: Double;
begin
  {$ifdef PAS2JS}Result := GStatus.offsetHeight;
  {$else}Result := GStatus.Height;{$endif}
end;

function ButtonCaption: TNyxText;
begin
  {$ifdef PAS2JS}Result := GButton.textContent;
  {$else}Result := TNyxText(GButton.Caption);{$endif}
end;

function ButtonEnabled: Boolean;
begin
  {$ifdef PAS2JS}Result := not GButton.disabled;
  {$else}Result := GButton.Enabled;{$endif}
end;

procedure Step;
begin
  case GStage of
    0:
      begin
        GApp.View.Sync;
        {$ifdef PAS2JS}
        GArea := GApp.View.ElementFor('available');
        GStatus := GApp.View.ElementFor('status');
        GButton := TJSHTMLButtonElement(GApp.View.ElementFor('detail-action'));
        GNotes := TJSHTMLTextAreaElement(GApp.View.InputFor('notes'));
        {$else}
        GArea := TWinControl(GApp.View.ControlFor('available'));
        GStatus := TLabel(GApp.View.ControlFor('status'));
        GButton := TNyxLCLButton(GApp.View.ControlFor('detail-action'));
        GNotes := TMemo(GApp.View.InputFor('notes'));
        {$endif}
        GAreaHeight := AreaHeight;
        GLabelHeight := LabelHeight;
        Check((GAreaHeight > CThreshold) and (ButtonCaption = 'Open details') and ButtonEnabled,
          'short status leaves real allocated space for the enabled detail action');
        {$ifdef PAS2JS}GButton.textContent := 'Host sentinel';
        {$else}GButton.Caption := 'Host sentinel';{$endif}
        GApp.State.SetValue(NyxTextState('status'), 'Still ready');
        Check(ButtonCaption = 'Host sentinel', 'ordinary status update initially uses selective publication');
        {$ifdef PAS2JS}
        GButton.textContent := 'Open details';
        GNotes.focus;
        GNotes.value := CDraft;
        GNotes.selectionStart := 3;
        GNotes.selectionEnd := 6;
        {$else}
        GButton.Caption := 'Open details';
        GNotes.SetFocus;
        GNotes.Text := CDraft;
        GNotes.SelStart := 3;
        GNotes.SelLength := 3;
        {$endif}
        GApp.State.SetValue(NyxTextState('status'), CLong);
        Check((AreaHeight > 0) and (AreaHeight < CThreshold) and
          (AreaHeight < GAreaHeight) and (LabelHeight > GLabelHeight),
          'wrapped text takes actual positive height from the size-contained fill region');
        Check((ButtonCaption = 'More room needed') and not ButtonEnabled,
          'container caption/interaction settle synchronously before queued observers run');
      end;
    1:
      begin
        {$ifdef PAS2JS}
        Check((GApp.View.ElementFor('available') = GArea) and
          (GApp.View.ElementFor('detail-action') = GButton) and
          (GApp.View.InputFor('notes') = GNotes), 'browser container publication retains physical faces');
        Check((document.activeElement = GNotes) and (GNotes.value = CDraft) and
          (GNotes.selectionStart = 3) and (GNotes.selectionEnd = 6),
          'browser container changes retain editor draft, focus and selection');
        {$else}
        Check((GApp.View.ControlFor('available') = GArea) and
          (GApp.View.ControlFor('detail-action') = GButton) and
          (GApp.View.InputFor('notes') = GNotes), 'native container publication retains physical faces');
        Check((Screen.ActiveControl = GNotes) and (TNyxText(GNotes.Text) = CDraft) and
          (GNotes.SelStart = 3) and (GNotes.SelLength = 3),
          'native container changes retain editor text, focus and selection');
        {$endif}
        GApp.State.SetValue(NyxTextState('status'), 'Ready');
        Check((AreaHeight > CThreshold) and (ButtonCaption = 'Open details') and ButtonEnabled,
          'shortened text synchronously restores the larger-container presentation');
      end;
    2:
      begin
        Check((Abs(AreaHeight - GAreaHeight) < 1) and (LabelHeight = GLabelHeight),
          'ordinary layout returns to the original allocated dimensions');
        Check(TNyxCodec.Encode(GDocument) = GOriginal,
          'runtime wrapping and named container rules preserve authored document bytes');
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive; forward;

{ Real animation frames settle mounting/queued work between steps. Each state
  write's immediate assertions above run before another frame can conceal it. }
procedure ScheduleDrive;
begin
  window.requestAnimationFrame(procedure(ATime: Double)
    begin
      window.requestAnimationFrame(procedure(ANext: Double)
        begin
          Drive;
        end);
    end);
end;

procedure Drive;
begin
  try
    Step;

    if (GStage = 1) and GCapture then
    begin
      document.body.setAttribute('data-container-stage', 'compact-ready');
      GContinue.disabled := False;
      Exit;
    end;

    if GStage = 3 then
    begin
      document.body.setAttribute('data-container-controls', 'passed');
      document.body.setAttribute('data-container-checks', IntToStr(GChecks));
      GFinish.disabled := False;
      Exit;
    end;
    ScheduleDrive;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-container-controls', 'failed');
      document.body.setAttribute('data-container-error', LException.Message);
      GFinish.disabled := False;
    end;
  end;
end;

function ContinueChecks(AEvent: TJSMouseEvent): Boolean;
begin
  GCapture := False;
  GContinue.disabled := True;
  ScheduleDrive;
  Result := True;
end;

{ The review-only pause must not move focus away from the editor whose retained
  focus is asserted by the next step. Suppress this harness button's pointer
  default; ordinary automatic regression runs never enter the capture pause. }
function KeepEditorFocus(AEvent: TJSMouseEvent): Boolean;
begin
  AEvent.preventDefault;
  Result := False;
end;

function Finish(AEvent: TJSMouseEvent): Boolean;
begin
  Retire;
  document.body.setAttribute('data-container-retired', 'true');
  Result := True;
end;

var
  LHost: TJSHTMLElement;
{$else}
var
  LTurn: Integer;
{$endif}

begin
  try
    GDocument := Fixture;
    GOriginal := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GCapture := window.location.search = '?capture=1';
    LHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHost);
    GApp := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, LHost);
    GContinue := TJSHTMLButtonElement(document.createElement('button'));
    GContinue.textContent := 'Continue container checks';
    GContinue.disabled := True;
    GContinue.onmousedown := @KeepEditorFocus;
    GContinue.onclick := @ContinueChecks;
    document.body.appendChild(GContinue);
    GFinish := TJSHTMLButtonElement(document.createElement('button'));
    GFinish.textContent := 'Finish container checks';
    GFinish.disabled := True;
    GFinish.onclick := @Finish;
    document.body.appendChild(GFinish);
    ScheduleDrive;
    {$else}
    Application.Initialize;
    Application.CaptureExceptions := False;
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
    GApp.Window.ClientHeight := 844;
    GApp.Window.Show;
    while GStage < 3 do
    begin
      for LTurn := 1 to 5 do
      begin
        Application.ProcessMessages;
        Application.Idle(False);
        CheckSynchronize(0);
      end;
      Step;
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual container state checks');
    {$endif}
  except
    on LException: Exception do
    begin
      Retire;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-container-controls', 'failed');
      document.body.setAttribute('data-container-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
