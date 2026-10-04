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
program nyx_mcp_callback_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, base64, nyx.text, nyx.data, nyx.test.mcp.client,
  nyx.test.browser.host;

type
  { Semantic tools own all authoring. CDP reads small Pascal-published observer
    attributes and captures the final UI; it never injects code or drives the
    designer. The already-open Nyx editor qualifies visibility and ordinary undo. }
  TCallbackJourney = class(TNyxBrowserHost)
  private
    FClient: TNyxMCPTestClient;
    FRevision: Integer;
    FCount: Integer;
    function Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
    function Args(const AID, AChanges: TNyxText; const AReview: TNyxText = ''): TNyxDataValue;
    procedure WaitPhase(const APhase: TNyxText);
    procedure ExportSource(const ADirectory: String);
    procedure Check(AValue: Boolean; const AReason: TNyxText);
  public
    procedure Run(AClient: TNyxMCPTestClient; const AExport: String);
  end;

procedure TCallbackJourney.Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(FCount);
end;

function TCallbackJourney.Call(const ATool: TNyxText; const AArgs: TNyxDataValue): TNyxDataValue;
var
  LValue: TNyxDataValue;
begin
  LValue := FClient.Tool(ATool, AArgs);

  if LValue.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic call refused: ' + LValue.ToJSON);
  end;
  Result := LValue.Field('structuredContent');

  if Result.Field('revision').Defined then
  begin
    FRevision := Result.Field('revision').AsInteger;
  end;
end;

function TCallbackJourney.Args(const AID, AChanges: TNyxText; const AReview: TNyxText): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 3);
  LFields[0] := NyxField('expectedRevision', NyxData(FRevision));
  LFields[1] := NyxField('mode', NyxData('review'));
  LFields[2] := NyxField('changes', TNyxDataValue.ParseJSON(AChanges));

  if AID <> '' then
  begin
    LFields[1] := NyxField('mode', NyxData('apply'));
    SetLength(LFields, 4);
    LFields[3] := NyxField('operationId', NyxData(AID));
  end;

  if AReview <> '' then
  begin
    SetLength(LFields, 5);
    LFields[4] := NyxField('reviewID', NyxData(AReview));
  end;
  Result := NyxObject(LFields);
end;

procedure TCallbackJourney.WaitPhase(const APhase: TNyxText);
var
  LStarted: QWord;
  LPhase: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LPhase := Attribute('data-callback-phase');

    if LPhase = 'failed' then
    begin
      raise Exception.Create('Nyx observer failed: ' + Attribute('data-callback-error'));
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      raise Exception.Create('Nyx callback observer timeout waiting for ' + APhase + '; observed ' + LPhase);
    end;
    Sleep(20);
  until LPhase = APhase;
  Check(True, 'Observed phase ' + APhase);
end;

procedure TCallbackJourney.ExportSource(const ADirectory: String);
var
  LLines: TNyxStrings;
  LValue: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
  LFile: TFileStream;
  LText: TNyxText;
begin
  ForceDirectories(ADirectory);
  LRevision := FRevision;
  LLine := 1;
  LLines := TNyxStrings.Create;
  try
    repeat
      LValue := Call('nyx_source', NyxObject([NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));
      Check(FRevision = LRevision, 'Source export retains one revision');
      for LIndex := 0 to LValue.Field('lines').Count - 1 do
      begin
        LLines.Add(LValue.Field('lines').Item(LIndex).AsText);
      end;
      Inc(LLine, LValue.Field('lines').Count);
    until LLine > LValue.Field('totalLines').AsInteger;
    LText := LLines.Text;
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + 'nyx.generated.view.pas', fmCreate);
    try
      LFile.WriteBuffer(LText[1], Length(LText));
    finally
      LFile.Free;
    end;
  finally
    LLines.Free;
  end;
end;

procedure TCallbackJourney.Run(AClient: TNyxMCPTestClient; const AExport: String);
var
  LValue: TNyxDataValue;
  LArgs: TNyxDataValue;
  LFirst: TNyxText;
  LSecond: TNyxText;
  LRemove: TNyxText;
  LReview: TNyxText;
  LCapture: TNyxDataValue;
begin
  FClient := AClient;
  Call('nyx_session', NyxObject([]));
  WaitPhase('ready');
  LArgs := Args('callback-add',
    '[{"op":"add","id":"callback-apply","event":{"trigger":"click"}},' +
    '{"op":"add","id":"callback-apply","event":{"trigger":"click"}},' +
    '{"op":"add","id":"callback-notes","event":{"trigger":"after-key-press"}},' +
    '{"op":"add","id":"callback-search","event":{"name":"search"}}]');
  LValue := Call('nyx_callbacks', LArgs);
  Check(LValue.Field('callbacks').Count = 4, 'Physical and named callbacks authored in one semantic batch');
  LFirst := LValue.Field('callbacks').Item(0).Field('registration').AsText;
  LSecond := LValue.Field('callbacks').Item(1).Field('registration').AsText;
  Check(Call('nyx_callbacks', LArgs).ToJSON = LValue.ToJSON, 'Actual HTTP retry does not duplicate callback stubs');
  Command('DOM.setAttributeValue', NyxObject([NyxField('nodeId', NyxData(Node('body'))),
    NyxField('name', NyxData('data-callback-second')), NyxField('value', NyxData(LSecond))]));
  WaitPhase('added');
  Call('nyx_callbacks', Args('callback-order',
    '[{"op":"move","id":"callback-apply","event":{"trigger":"click"},"registration":"' + LSecond + '","index":0},' +
    '{"op":"policy","id":"callback-apply","event":{"trigger":"click"},"policy":"ui-queue"}]'));
  WaitPhase('moved');
  ExportSource(IncludeTrailingPathDelimiter(AExport) + 'ordered');
  LRemove := '[{"op":"remove","id":"callback-apply","event":{"trigger":"click"},"registration":"' + LSecond + '"}]';
  Check(FClient.Tool('nyx_callbacks', Args('blind-remove', LRemove)).Field('isError').AsBoolean,
    'Authenticated removal still requires review');
  LValue := Call('nyx_callbacks', Args('', LRemove));
  LReview := LValue.Field('reviewID').AsText;
  Check(Pos('implementation is retained', LValue.Field('callbacks').Item(0).Field('warning').AsText) > 0,
    'Actual review supplies the retained-code warning');
  Call('nyx_callbacks', Args('callback-remove', LRemove, LReview));
  WaitPhase('undone');
  Call('nyx_session', NyxObject([]));
  Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(FRevision)),
    NyxField('operationId', NyxData('callback-redo')), NyxField('direction', NyxData('redo'))]));
  WaitPhase('passed');
  LValue := Call('nyx_node', NyxObject([NyxField('id', NyxData('callback-apply')),
    NyxField('events', NyxData(True)), NyxField('eventLimit', NyxData(1)),
    NyxField('keys', NyxArray([NyxData('text')]))]));
  Check(Pos(LFirst, LValue.ToJSON) > 0, 'Bounded semantic context retains surviving registration');
  ExportSource(IncludeTrailingPathDelimiter(AExport) + 'removed');
  Save('result.json', NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('nativeChecks', NyxData(FCount)), NyxField('browserChecks', NyxData(Attribute('data-nyx-callback-checks'))),
    NyxField('first', NyxData(LFirst)), NyxField('second', NyxData(LSecond))]).ToJSON);
  LCapture := Command('Page.captureScreenshot', NyxObject([]));
  Save('capture.png', DecodeStringBase64(LCapture.Field('data').AsText));
  WriteLn('PASS ', FCount, ' actual semantic callback/observer checks');
end;

var
  LClient: TNyxMCPTestClient;
  LDriver: TCallbackJourney;
  LValue: TNyxDataValue;
  LRevision: Integer;
begin
  LClient := nil;
  LDriver := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply isolated loopback editor base, MCP config, artifact directory and source export directory');
    end;
    LClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty callback review');
    LValue := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
    LRevision := LValue.Field('revision').AsInteger;
    LValue := LClient.Tool('nyx_transaction', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('callback-compose')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"page","id":"callback-review","root":"page","properties":{"padding":24,"gap":14}},' +
        '{"op":"create","kind":"button","id":"callback-apply","parent":"callback-review","properties":{"text":"Apply idea"}},' +
        '{"op":"create","kind":"memo","id":"callback-notes","parent":"callback-review","properties":{"text":"Notes"}},' +
        '{"op":"create","kind":"search-field","id":"callback-search","parent":"callback-review"}]'))]));

    if LValue.Field('isError').AsBoolean then
    begin
      raise Exception.Create('Review composition refused: ' + LValue.ToJSON);
    end;
    LRevision := LValue.Field('structuredContent').Field('revision').AsInteger;
    LValue := LClient.Tool('nyx_select', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('callback-view')), NyxField('id', NyxData('callback-review')),
      NyxField('activate', NyxData(True))]));
    LRevision := LValue.Field('structuredContent').Field('revision').AsInteger;
    LClient.Tool('nyx_select', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('callback-selection')), NyxField('id', NyxData('callback-apply'))]));
    LDriver := TCallbackJourney.Create(ParamStr(1) + '/agent-callback-observer.html', ParamStr(3));
    LDriver.Run(LClient, ParamStr(4));
    FreeAndNil(LDriver);
    LClient.Close;
    FreeAndNil(LClient);
  except
    on LException: Exception do
    begin
      FreeAndNil(LDriver);
      FreeAndNil(LClient);
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
