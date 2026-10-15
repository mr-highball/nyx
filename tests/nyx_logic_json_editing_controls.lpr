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
program nyx_logic_json_editing_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.test.logic.json.editing
  {$ifdef NYX_JSON_EDITING_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, StdCtrls, Graphics, nyx.widgets.lcl,
    nyx.application.lcl{$endif};

var
  GDocument: TNyxDocument;
  GExpected: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GFinished: Boolean;
  GResult: TNyxText;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GStarted: QWord;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  GApp.View.ElementFor('run-button').click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
  {$endif}
end;

procedure Input(const AID, AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GApp.View.InputFor(AID)).value := AText;
  GApp.View.InputFor(AID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor(AID)).Text := AText;
  {$endif}
end;

function Idle: Boolean;
begin
  Result := GApp.FlowRoutes.Report(0).Field('active').AsInteger = 0;
end;

procedure Step;
var
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
  LText: TNyxText;
begin
  case GStage of
    0:
    begin
      Check(GApp.FlowRoutes.Count = 1, 'Ordinary application mounted the saved request route');
      Click;
    end;
    1:
    begin

      if not Idle then
      begin
        Exit;
      end;
      LText := GApp.State.GetValue(NyxTextState('result'));
      Check(TNyxDataValue.ParseJSON(LText).ToJSON =
        TNyxDataValue.ParseJSON(JSONEditingExpected).ToJSON, 'Compiled connected operations produce exact request');
      Check(GOther.State.GetValue(NyxTextState('result')) <> LText, 'Independent application state');
      Input('name-memo', '"Linus"');
      Check(GApp.State.GetValue(NyxTextState('name')) = '"Linus"', 'Actual memo input updates the bound state');
      Click;
    end;
    2:
    begin

      if not Idle then
      begin
        Exit;
      end;
      GResult := GApp.State.GetValue(NyxTextState('result'));
      Check(TNyxDataValue.ParseJSON(GResult).Field('profile').Field('name').AsText = 'Linus',
        'Dynamic replacement reaches nested JSON field');
      Check(Pos('9007199254740993', GResult) > 0, 'Exact decimal survives all connected transformations');
      {$ifdef PAS2JS}
      LText := GApp.View.ElementFor('result-code').textContent;
      {$else}
      LText := TNyxText(TMemo(GApp.View.ControlFor('result-code')).Text);
      {$endif}
      Check((Pos('Linus', LText) > 0) and (Pos('9007199254740993', LText) > 0),
        'Ordinary bound code control displays complete output');
      Input('patch-memo', '{"profile":null}');
      Click;
    end;
    3:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.FlowRoutes.Report(0).Field('failed').AsInteger = 1) and
        (GApp.FlowRoutes.Report(0).Field('failure').AsText <> ''), 'Wrong parent shape is an inspectable failure');
      Check(GApp.State.GetValue(NyxTextState('result')) = GResult, 'Failed graph preserves previous output');
      Input('patch-memo', JSONEditingPatch);
      Input('name-memo', '"Cancelled result"');
      Click;
      GApp.FlowRoutes.Cancel(0);
    end;
    4:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GApp.State.GetValue(NyxTextState('result')) = GResult, 'Cancellation prevents publication');
      Input('name-memo', '"Retired result"');
      Click;
      GApp.ShowPage('other');
    end;
    5:
    begin
      Check(GApp.State.GetValue(NyxTextState('result')) = GResult, 'Navigation retires queued output');
      GApp.ShowPage('home');
      Input('name-memo', '"Fresh request"');
      Click;
    end;
    6:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(Pos('Fresh request', GApp.State.GetValue(NyxTextState('result'))) > 0,
        'Fresh generation executes the same saved graph');
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Runtime leaves saved source/defaults/graph unchanged');
      {$ifdef PAS2JS}
      window.scrollTo(0, 0);
      document.body.setAttribute('data-capture-checkpoint', 'json-request');
      {$else}

      if ParamCount > 0 then
      begin
        LBitmap := GApp.Window.GetFormImage;
        LPNG := TPortableNetworkGraphic.Create;
        try
          LPNG.Assign(LBitmap);
          LPNG.SaveToFile(ParamStr(1));
        finally
          LPNG.Free;
          LBitmap.Free;
        end;
      end;
      {$endif}
    end;
    7:
    begin
      {$ifdef PAS2JS}

      if document.body.getAttribute('data-capture-observed') <> 'json-request' then
      begin
        Exit;
      end;
      {$endif}
      GFinished := True;
    end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 60000 then
    begin
      raise Exception.Create('JSON request control deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-json-editing-controls', 'passed');
      document.body.setAttribute('data-json-editing-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-json-editing-controls', 'failed');
      document.body.setAttribute('data-json-editing-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    {$ifdef NYX_JSON_EDITING_COMPANION}
    GDocument := BuildNyxDocument;
    GExpected := JSONEditingDocument;
    try
      Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
        'Real compiler reconstructs the exact whole request-builder document');
    finally
      FreeAndNil(GExpected);
    end;
    {$else}
    GDocument := JSONEditingDocument;
    {$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
    GOtherHost.style.setProperty('display', 'none');
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GOther.Run(GDocument, GOtherHost);
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GOther.Mount(GDocument);
    GApp.Window.SetBounds(10, 10, 760, 900);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;

      if GetTickCount64 - GStarted > 60000 then
      begin
        raise Exception.Create('Native JSON request control deadline at stage ' + IntToStr(GStage));
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' compiled JSON editing application controls');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-json-editing-controls', 'failed');
      document.body.setAttribute('data-json-editing-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
