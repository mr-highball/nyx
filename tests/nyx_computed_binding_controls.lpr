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

program nyx_computed_binding_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.application.browser,{$else}
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, nyx.application.lcl,{$endif}
  {$ifdef PAS2JS}SysUtils,{$endif}
  nyx.text, nyx.types, nyx.state, nyx.expressions, nyx.model, nyx.codec,
  nyx.generated.view;

var
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GNotes: TJSHTMLTextAreaElement;
  GLabel: TJSHTMLElement;
  GCapture: Boolean;
  {$else}
  GApplication: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GNotes: TMemo;
  GLabel: TControl;
  {$endif}
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GFinished: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Computed application controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function Caption(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GApplication.View.ElementFor(AID).textContent;
  {$else}
  Result := TNyxText(TLabel(GApplication.View.ControlFor(AID)).Caption);
  {$endif}
end;

function Enabled: Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(GApplication.View.ElementFor('submit')).disabled;
  {$else}
  Result := GApplication.View.ControlFor('submit').Enabled;
  {$endif}
end;

function Notes: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GNotes.value;
  {$else}
  Result := TNyxText(GNotes.Text);
  {$endif}
end;

procedure Checkpoint(const AName: TNyxText);
begin
  {$ifdef PAS2JS}

  if GCapture then
  begin
    document.body.setAttribute('data-capture-checkpoint', AName);
  end;
  {$endif}
end;

procedure Step;
var
  LRefused: Boolean;
  LRevision: Integer;
begin
  case GStage of
    0:
      begin
        Check(Caption('total') = 'Total: 40', 'compiled typed formula reaches an actual caption');
        Check(Enabled, 'compiled Boolean condition reaches the actual button');
        {$ifdef PAS2JS}
        GLabel := GApplication.View.ElementFor('total');
        GNotes := TJSHTMLTextAreaElement(GApplication.View.InputFor('notes'));
        GNotes.focus;
        GNotes.value := 'Unfinished order notes';
        {$else}
        GLabel := GApplication.View.ControlFor('total');
        GNotes := TMemo(GApplication.View.InputFor('notes'));
        GNotes.SetFocus;
        GNotes.Text := 'Unfinished order notes';
        {$endif}
        GApplication.State.SetValue(NyxIntegerState('quantity'), 0);
      end;
    1:
      begin
        Check((Caption('total') = 'Total: 2.5') and not Enabled,
          'one runtime key update changes actual calculated caption and enabled state');
        {$ifdef PAS2JS}
        Check((GApplication.View.ElementFor('total') = GLabel) and
          (GApplication.View.InputFor('notes') = GNotes), 'browser keeps physical controls through recomputation');
        Check(document.activeElement = GNotes, 'browser retains the unrelated editor focus');
        {$else}
        Check((GApplication.View.ControlFor('total') = GLabel) and
          (GApplication.View.InputFor('notes') = GNotes), 'native keeps physical controls through recomputation');
        Check(Screen.ActiveControl = GNotes, 'native retains the unrelated editor focus');
        {$endif}
        Check(Notes = 'Unfinished order notes', 'unrelated accepted key change retains physical memo text');
        Checkpoint('computed-live-update');
      end;
    2:
      begin
        LRevision := GApplication.State.Revision;
        LRefused := False;
        try
          GApplication.State.Apply([
            NyxStateValue(NyxIntegerState('quantity'), 2),
            NyxStateValue(NyxNumberState('price'), 1.7e308)]);
        except
          on ENyxExpression do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (GApplication.State.Revision = LRevision) and
          (Caption('total') = 'Total: 2.5') and not Enabled and
          (Notes = 'Unfinished order notes'),
          'invalid arithmetic preserves accepted state and every physical consumer');
        GApplication.State.Apply([
          NyxStateValue(NyxIntegerState('quantity'), 4),
          NyxStateValue(NyxBooleanState('ready'), False)]);
      end;
    3:
      begin
        Check((Caption('total') = 'Total: 52.5') and not Enabled,
          'grouped updates publish the final computed presentation');
        Check(Notes = 'Unfinished order notes', 'grouped recomputation retains unrelated editor text');
        GApplication.State.SetValue(NyxBooleanState('ready'), True);
        Check(Enabled, 'second condition input independently controls the actual button');
        {$ifdef PAS2JS}
        { The earlier stages deliberately retained an unfinished physical draft.
          Commit through the actual field event before retiring this page. }
        GNotes.dispatchEvent(TJSEvent.new('change'));
        {$endif}
        Check(GApplication.State.GetValue(NyxTextState('notes')) =
          'Unfinished order notes', 'ordinary field event commits into its direct typed state');
        GApplication.ShowPage('review');
      end;
    4:
      begin
        Check(Caption('review-total') = 'Total: 52.5',
          'navigation projects retained runtime values on another page');
        GApplication.State.SetValue(NyxIntegerState('quantity'), 1);
        Check(Caption('review-total') = 'Total: 15',
          'unmounted-page validation and mounted computation share the application store');
        GApplication.ShowPage('home');
      end;
    5:
      begin
        Check((Caption('total') = 'Total: 15') and Enabled,
          'return navigation reconstructs computed caption and condition');
        {$ifdef PAS2JS}
        GNotes := TJSHTMLTextAreaElement(GApplication.View.InputFor('notes'));
        {$else}
        GNotes := TMemo(GApplication.View.InputFor('notes'));
        {$endif}
        Check(Notes = 'Unfinished order notes',
          'return navigation restores the direct field alongside computed values');
        Check(GOther.State.GetValue(NyxIntegerState('quantity')) = 3,
          'second application owns independent expression inputs');
        {$ifdef PAS2JS}
        Check(GOther.View.ElementFor('total').textContent = 'Total: 40',
          'second browser application retains independent physical consumers');
        {$else}
        Check(TNyxText(TLabel(GOther.View.ControlFor('total')).Caption) = 'Total: 40',
          'second native application retains independent physical consumers');
        {$endif}
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'actual controls and navigation never rewrite authored defaults');
        Checkpoint('computed-navigation');
      end;
    6:
      begin
        GFinished := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApplication);
  FreeAndNil(GOther);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if GCapture and (document.body.getAttribute('data-capture-checkpoint') <> '') and
      (document.body.getAttribute('data-capture-observed') <>
      document.body.getAttribute('data-capture-checkpoint')) then
    begin
      window.setTimeout(@Drive, 50);
      Exit;
    end;
    Step;

    if GFinished then
    begin
      Retire;
      document.body.setAttribute('data-computed-controls', 'passed');
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' actual computed application checks';
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-computed-controls', 'failed');
      document.body.setAttribute('data-computed-error', LException.Message);

      if not GCapture then
      begin
        Retire;
      end;
    end;
  end;
end;
{$endif}

begin
  try
    GDocument := BuildNyxDocument;
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GCapture := window.location.search = '?capture=1';
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
    GApplication := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GApplication.Run(GDocument, GHost);
    GOther.Run(GDocument, GOtherHost);
    Drive;
    {$else}
    Application.Initialize;
    GApplication := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GApplication.Mount(GDocument);
    GOther.Mount(GDocument);
    GOther.Window.Show;
    GApplication.Window.Show;
    while not GFinished do
    begin
      Application.ProcessMessages;
      Step;
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual computed application checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-computed-controls', 'failed');
      document.body.setAttribute('data-computed-error', LException.Message);
      {$else}
      Retire;
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
