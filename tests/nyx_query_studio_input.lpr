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



program nyx_query_studio_input;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, nyx.text, nyx.collections.query.editor, nyx.studio.authoring,
  nyx.studio.inspector, nyx.test.browser.pipe;

const
  CEditor = 'inspector-collection-query';
var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;
  GWidth: Integer;
  GStarted: QWord;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Actual query Studio input: ' + AReason);
  end;
  Inc(GChecks);
end;

function Node(const AID: TNyxText): TNyxText;
begin
  Result := '[data-node="' + AID + '"]';
end;

function Input: TNyxText;
var
  LID: TNyxText;
begin
  LID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue);
  Result := 'input' + Node(LID) + ', ' + Node(LID) + ' input';
end;

procedure WaitStage(const AStage: TNyxText);
var
  LStarted: QWord;
  LState: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    LState := GHost.Attribute('data-query-studio');

    if LState = 'failed' then
    begin
      GHost.Capture('failure');
      raise Exception.Create(GHost.Attribute('data-event-error'));
    end;

    if GHost.RuntimeError <> '' then
    begin
      GHost.CaptureRuntimeError;
      raise Exception.Create('Browser exception; inspect owned runtime-error.json');
    end;

    if GHost.Attribute('data-query-studio-stage') = AStage then
    begin
      WriteLn('Observed ordinary query stage / ', AStage);
      Flush(Output);
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      GHost.Capture('stage-timeout');
      raise Exception.Create('Ordinary query stage did not finish: ' + AStage);
    end;
    Sleep(25);
  until False;
end;

procedure Acknowledge(const AAction: TNyxText);
begin
  GHost.SetAttribute('data-query-studio-action', AAction);
end;

procedure Enter(const AText: TNyxText);
var
  LValue: TNyxText;
begin
  GHost.ReplaceText(Input, AText);
  GHost.Tab;
  Check(GHost.TryFieldValue(Input, LValue) and (LValue = AText),
    'trusted host input retains ' + AText);
  GHost.Click(Node(NyxQueryEditorPreviewID(CEditor)));
end;

procedure CaptureReport(const AName: String);
var
  LBox: TNyxBrowserBox;
begin
  GHost.Reveal(Node(NyxQueryEditorPreviewOutputID(CEditor)));
  LBox := GHost.Bounds(Node(NyxQueryEditorPreviewOutputID(CEditor)));
  Check((LBox.Width > 0) and (LBox.Height >= 200), 'report has useful physical space');
  Check((LBox.Top >= 0) and (LBox.Top + LBox.Height <= 900),
    'report is physically inside the observing viewport');
  GHost.Capture(AName);
end;

begin
  GHost := nil;
  try
    Check(ParamCount = 3, 'supply owned URL, new capture directory and width');
    GWidth := StrToInt(ParamStr(3));
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), GWidth, 900);
    WaitStage('open');

    if GHost.Exists(Node('action-panel-inspector')) and
      (GHost.Bounds(Node('action-panel-inspector')).Height > 0) then
    begin
      GHost.Click(Node('action-panel-inspector'));
    end;
    GHost.Click(Node(NyxStudioBindingsToggleID));
    Acknowledge('opened');
    WaitStage('text');
    Enter('Plan');
    Acknowledge('text-preview');
    WaitStage('park');

    if GHost.Exists(Node('action-panel-design')) then
    begin
      GHost.Click(Node('action-panel-design'));
    end;
    GStarted := GetTickCount64;
    while not GHost.Exists(Node('measured-table')) do
    begin

      if GetTickCount64 - GStarted > 5000 then
      begin
        raise Exception.Create('Accepted canvas did not mount for physical observation');
      end;
      Sleep(25);
    end;
    Check(Pos('Explore the collection', GHost.ElementHTML(Node('measured-table'))) > 0,
      'physical canvas retains the accepted text filter');

    if GHost.Exists(Node('action-panel-inspector')) then
    begin
      GHost.Click(Node('action-panel-inspector'));
    end;
    GHost.Click(Node(NyxInspectorEventsID));
    GHost.Click(Node(NyxInspectorPropertiesID));
    Acknowledge('parked');
    WaitStage('text-capture');
    CaptureReport('query-text');
    Acknowledge('captured-text');
    WaitStage('number');
    Enter('-');
    Acknowledge('malformed');
    WaitStage('correct');
    Enter('0');
    Acknowledge('corrected');
    WaitStage('number-capture');
    CaptureReport('query-number');

    if GHost.Exists(Node('action-panel-design')) then
    begin
      GHost.Click(Node('action-panel-design'));
    end;
    Acknowledge('captured-number');
    WaitStage('return');

    if GHost.Exists(Node('action-panel-inspector')) then
    begin
      GHost.Click(Node('action-panel-inspector'));
    end;
    WaitStage('done');
    Check(GHost.Attribute('data-query-studio') = 'passed', 'ordinary fixture reaches actual success');
    Check(GHost.Attribute('data-query-studio-disposed') = 'true', 'ordinary controller explicitly retires');
    WriteLn('PASS ', GChecks, ' trusted host checks / ',
      GHost.Attribute('data-query-studio-checks'), ' ordinary controller checks / width ', GWidth);
  except
    on LException: Exception do
    begin

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GHost.Free;
end.
