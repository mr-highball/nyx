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
program nyx_binding_projection_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.expressions, nyx.responsive, nyx.interaction.policy,
  {$ifdef PAS2JS}JS, Web, nyx.application.browser;
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, nyx.application.lcl;
  {$endif}

const
  CLong: TNyxText = 'A changing status should wrap naturally and move the next ' +
    'control down without replacing the editor beside it. Exact text: 🌙.';

type
  { A pure runtime constraint owns no tree/control. Attaching and releasing it
    tests effective interaction changes which never rewrite stored properties. }
  TProjectionReadOnlyPolicy = class(TInterfacedObject, INyxRuntimeReadOnlyPolicy)
  public
    function GetReadOnly: Boolean;
  end;
  { A deterministic host reaction retires the actual view during selective
    publication. Node is borrowed only inside that entered setter/layout frame.
    Browser instrumentation restores the original borrowed DOM method afterward;
    native instrumentation uses the real label's ordinary resize event. }
  TProjectionRetirement = class
  public
    Node: TNyxNode;
    Armed: Boolean;
    Calls: Integer;
    SawEnteredNode: Boolean;
    {$ifdef PAS2JS}
    Element: TJSHTMLElement;
    OriginalSetter: TJSFunction;
    procedure SetAttribute(const AName, AValue: String);
    {$else}
    procedure Resized(ASender: TObject);
    {$endif}
    procedure RetireView;
  end;

var
  GChecks: Integer;
  GStage: Integer;
  GHeight: Integer;
  GTop: Integer;
  GDocument: TNyxDocument;
  GReaction: TProjectionRetirement;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GPeer: TNyxBrowserApplication;
  GLabel: TJSHTMLElement;
  GMemo: TJSHTMLTextAreaElement;
  {$else}
  GApp: TNyxLCLApplication;
  GPeer: TNyxLCLApplication;
  GLabel: TControl;
  GMemo: TMemo;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Projection controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function TProjectionReadOnlyPolicy.GetReadOnly: Boolean;
begin
  Result := True;
end;

procedure TProjectionRetirement.RetireView;
begin

  if not Armed then
  begin
    Exit;
  end;
  Armed := False;
  Inc(Calls);
  GApp.View.Unmount;
  SawEnteredNode := (Node.ID = 'status') and (Node.Prop('text') = CLong);
end;

{$ifdef PAS2JS}
procedure TProjectionRetirement.SetAttribute(const AName, AValue: String);
begin
  OriginalSetter.call(Element, AName, AValue);

  if AName = 'aria-label' then
  begin
    RetireView;
  end;
end;
{$else}
procedure TProjectionRetirement.Resized(ASender: TObject);
begin
  RetireView;
end;
{$endif}

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LLabel: INyxLabel;
  LMemo: INyxMemo;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Live status';
    Result.State.SetValue(NyxTextState('status'), 'Ready')
      .SetValue(NyxTextState('hint'), 'Initial hint')
      .SetValue(NyxBooleanState('visible'), True)
      .SetValue(NyxBooleanState('readonly'), False)
      .SetValue(NyxTextState('notes'), 'Notes');
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Padding(12).Gap(10).Done;
    Result.AddPage(LPage);
    LLabel := NewNyxLabel('status');
    LLabel.Configure.Width(240).WhenViewport(TNyxViewportWidth.Below(600)).Width(160).Done;
    LLabel.Binds.Text(NyxTextState('status')).Hint(NyxTextState('hint')).Done;
    LPage.Add(LLabel);
    LLabel := NewNyxLabel('computed');
    LLabel.Configure.Width(260).Done;
    LLabel.Binds.Text(NyxTextExpression('Current: ').Append(
      NyxTextExpression(NyxTextState('status')))).Done;
    LPage.Add(LLabel);
    LLabel := NewNyxLabel('untouched');
    LLabel.WithText('Unchanged control');
    LPage.Add(LLabel);
    LLabel := NewNyxLabel('conditional');
    LLabel.WithText('Optional details');
    LLabel.Binds.Visible(NyxBooleanState('visible')).Done;
    LPage.Add(LLabel);
    LMemo := NewNyxMemo('notes');
    LMemo.Configure.Text('Working notes').Height(100).Done;
    LMemo.Binds.Value(NyxTextState('notes')).ReadOnly(NyxBooleanState('readonly')).Done;
    LPage.Add(LMemo);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function Caption(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GApp.View.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(TLabel(GApp.View.ControlFor(AID)).Caption);
  {$endif}
end;

procedure Step;
var
  LPolicy: INyxRuntimeReadOnlyPolicy;
  LNode: TNyxNode;
begin
  case GStage of
    0:
      begin
        Check(Caption('status') = 'Ready', 'initial actual text');
        {$ifdef PAS2JS}
        GLabel := GApp.View.ElementFor('status');
        GMemo := TJSHTMLTextAreaElement(GApp.View.InputFor('notes'));
        GHeight := Round(GLabel.offsetHeight);
        GTop := Round(GApp.View.ElementFor('computed').offsetTop);
        GMemo.focus;
        GMemo.value := 'Unfinished 🌙';
        GMemo.selectionStart := 3;
        GMemo.selectionEnd := 6;
        {$else}
        GLabel := GApp.View.ControlFor('status');
        GMemo := TMemo(GApp.View.InputFor('notes'));
        GHeight := GLabel.Height;
        GTop := GApp.View.ControlFor('computed').Top;
        GMemo.SetFocus;
        GMemo.Text := 'Unfinished 🌙';
        GMemo.SelStart := 3;
        GMemo.SelLength := 3;
        {$endif}
        GApp.State.Apply([NyxStateValue(NyxTextState('status'), CLong),
          NyxStateValue(NyxTextState('hint'), 'Current hint 🌙')]);
      end;
    1:
      begin
        Check((Caption('status') = CLong) and (Caption('computed') = 'Current: ' + CLong),
          'all computed current outputs reach actual controls');
        {$ifdef PAS2JS}
        Check((GApp.View.ElementFor('status') = GLabel) and
          (GApp.View.InputFor('notes') = GMemo), 'selective notification retains exact browser faces');
        Check((GLabel.offsetHeight > GHeight) and
          (GApp.View.ElementFor('computed').offsetTop > GTop), 'browser wraps and reallocates parent');
        Check(GLabel.title = 'Current hint 🌙', 'browser publishes changed hint');
        Check(not GLabel.hasAttribute('aria-label') or
          (GLabel.getAttribute('aria-label') = CLong), 'browser keeps native text accessibility naming');
        Check((document.activeElement = GMemo) and (GMemo.value = 'Unfinished 🌙') and
          (GMemo.selectionStart = 3) and (GMemo.selectionEnd = 6),
          'browser keeps unfinished draft, focus and selection');
        {$else}
        Check((GApp.View.ControlFor('status') = GLabel) and
          (GApp.View.InputFor('notes') = GMemo), 'selective notification retains exact native faces');
        Check((GLabel.Height > GHeight) and (GApp.View.ControlFor('computed').Top > GTop),
          'native wraps and reallocates parent');
        Check(TNyxText(GLabel.Hint) = TNyxText('Current hint 🌙'), 'native publishes changed hint');
        Check(TNyxText(GLabel.AccessibleName) = CLong, 'native updates text accessibility naming');
        Check((Screen.ActiveControl = GMemo) and (TNyxText(GMemo.Text) = TNyxText('Unfinished 🌙')) and
          (GMemo.SelStart = 3) and (GMemo.SelLength = 3), 'native keeps editor focus, text and selection');
        {$endif}
        Check(GPeer.State.GetValue(NyxTextState('status')) = 'Ready', 'independent runtime remains unchanged');
        { This deliberately changed borrowed host face is an observation marker,
          not an authored edit. A full replay would reset its static caption.
          The next ordinary text publication must leave that setter untouched. }
        {$ifdef PAS2JS}
        GApp.View.ElementFor('untouched').textContent := 'Host sentinel';
        {$else}
        TLabel(GApp.View.ControlFor('untouched')).Caption := 'Host sentinel';
        {$endif}
        GApp.State.SetValue(NyxTextState('status'), CLong + ' Updated.');
        Check(Caption('untouched') = 'Host sentinel',
          'ordinary admitted text changes do not replay an unrelated caption setter');
        {$ifdef PAS2JS}
        GApp.View.ElementFor('untouched').textContent := 'Unchanged control';
        {$else}
        TLabel(GApp.View.ControlFor('untouched')).Caption := 'Unchanged control';
        {$endif}
        GApp.State.Apply([NyxStateValue(NyxBooleanState('visible'), False),
          NyxStateValue(NyxBooleanState('readonly'), True)]);
      end;
    2:
      begin
        {$ifdef PAS2JS}
        Check((window.getComputedStyle(GApp.View.ElementFor('conditional')).getPropertyValue('display') = 'none') and GMemo.readOnly,
          'unsupported Boolean/read-only changes use complete target path');
        {$else}
        Check(not GApp.View.ControlFor('conditional').Visible and GMemo.ReadOnly,
          'unsupported Boolean/read-only changes use complete target path');
        {$endif}
        { A caller may configure the public runtime tree before a later state
          write. The target's previous copied observation must also see that
          unsupported width change, rather than trusting only this write's delta. }
        GApp.View.Root.Find('computed').Configure.Width(190).Done;
        GApp.State.SetValue(NyxTextState('status'), 'Ready again');
      end;
    3:
      begin
        Check((Caption('status') = 'Ready again') and (Caption('computed') = 'Current: Ready again'),
          'selective updates recover after a complete fallback');
        {$ifdef PAS2JS}
        Check(GLabel.offsetHeight = GHeight, 'browser returns intrinsic text height');
        Check(GApp.View.ElementFor('computed').offsetWidth = 190,
          'browser includes model edits preceding this state notification');
        {$else}
        Check(GLabel.Height = GHeight, 'native returns intrinsic text height');
        Check(GApp.View.ControlFor('computed').Width = 190,
          'native includes model edits preceding this state notification');
        {$endif}
        GApp.State.SetValue(NyxBooleanState('readonly'), False);
        LPolicy := TProjectionReadOnlyPolicy.Create;
        LNode := GApp.View.Root.Find('notes');
        LNode.AttachReadOnlyPolicy(LPolicy);
        try
          GApp.State.SetValue(NyxTextState('status'), 'Policy applied');
          Check(GMemo.ReadOnly,
            'policy restriction preceding a text write reaches the actual editor');
        finally
          LNode.DetachReadOnlyPolicy(LPolicy);
          LPolicy := nil;
        end;
        GApp.State.SetValue(NyxTextState('status'), 'Policy released');
        Check(not GMemo.ReadOnly,
          'policy removal preceding a text write restores actual editor interaction');
      end;
    4:
      begin
        GApp.View.Root.Find('status').Binds.AccessibleName(NyxTextState('hint')).Done;
        GApp.View.Sync;
        GReaction := TProjectionRetirement.Create;
        try
          GReaction.Node := GApp.View.Root.Find('status');
          {$ifdef PAS2JS}
          GReaction.Element := GLabel;
          GReaction.OriginalSetter := TJSFunction(TJSObject(GLabel)['setAttribute']);
          TJSObject(GLabel)['setAttribute'] := JSValue(@GReaction.SetAttribute);
          {$else}
          TLabel(GLabel).OnResize := GReaction.Resized;
          {$endif}
          GReaction.Armed := True;
          GApp.State.Apply([NyxStateValue(NyxTextState('status'), CLong),
            NyxStateValue(NyxTextState('hint'), 'Retiring view')]);
          Check((GReaction.Calls = 1) and GReaction.SawEnteredNode and
            (GApp.View.Root = nil), 'selective host reaction retains entered node and retires exact view');
          Check((GApp.State.GetValue(NyxTextState('status')) = CLong) and
            (GPeer.State.GetValue(NyxTextState('status')) = 'Ready'),
            'retirement retains committed state without affecting independent peer');
        finally
          GReaction.Armed := False;
          {$ifdef PAS2JS}
          TJSObject(GLabel)['setAttribute'] := GReaction.OriginalSetter;
          {$endif}
          GReaction.Node := nil;
          FreeAndNil(GReaction);
          GLabel := nil;
        end;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GPeer);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GStage = 5 then
    begin
      document.body.setAttribute('data-projection-controls', 'passed');
      document.body.setAttribute('data-projection-checks', IntToStr(GChecks));
      { The entered host reaction already retired its view. The final explicit
        click also disposes the independent application and document owners. }
      Exit;
    end;
    window.setTimeout(@Drive, 35);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-projection-controls', 'failed');
      document.body.setAttribute('data-projection-error', LException.Message);
    end;
  end;
end;

function Finish(AEvent: TJSMouseEvent): Boolean;
begin
  Retire;
  document.body.setAttribute('data-projection-retired', 'true');
  Result := True;
end;

var
  LHost: TJSHTMLElement;
  LPeerHost: TJSHTMLElement;
  LFinish: TJSHTMLButtonElement;
{$endif}

begin
  try
    GDocument := Fixture;
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('div'));
    LPeerHost := TJSHTMLElement(document.createElement('div'));
    LPeerHost.hidden := True;
    document.body.appendChild(LHost);
    document.body.appendChild(LPeerHost);
    GApp := TNyxBrowserApplication.Create;
    GPeer := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, LHost);
    GPeer.Run(GDocument, LPeerHost);
    LFinish := TJSHTMLButtonElement(document.createElement('button'));
    LFinish.textContent := 'Finish projection checks';
    LFinish.onclick := @Finish;
    document.body.appendChild(LFinish);
    Drive;
    {$else}
    Application.Initialize;
    Application.CaptureExceptions := False;
    GApp := TNyxLCLApplication.Create;
    GPeer := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GPeer.Mount(GDocument);
    GApp.Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
    GApp.Window.ClientHeight := 844;
    GPeer.Window.Show;
    GApp.Window.Show;
    while GStage < 5 do
    begin
      Application.ProcessMessages;
      Step;
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual projection control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      Retire;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-controls', 'failed');
      document.body.setAttribute('data-projection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
