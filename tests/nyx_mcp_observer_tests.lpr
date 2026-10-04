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

program nyx_mcp_observer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Process, nyx.text, nyx.data, nyx.studio.session,
  nyx.studio.projects, nyx.test.mcp.client;

var
  LClient: TNyxMCPTestClient;
  LSession: TNyxStudioSession;
  LProcess: TProcess;
  LValue: TNyxDataValue;
  LSummary: TNyxDataValue;
  LEditor: TNyxDataValue;
  LRevision: Integer;
  LBase: TNyxText;
  LDirectory: TNyxText;
  LOutput: TNyxText;
  LChunk: TNyxText;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LStarted: QWord;
  LPhase: Integer;
  LLog: TFileStream;
  LCount: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LCount);
end;

function Transaction(const AID, AOperations: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(ExtractFileName(ExcludeTrailingPathDelimiter(LDirectory)) + '-' + AID)),
    NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
end;

begin
  LClient := nil;
  LSession := nil;
  LProcess := nil;
  LPhase := 0;
  try
    LBase := ParamStr(1);
    LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    ForceDirectories(LDirectory);
    LSession := TNyxStudioSession.Create;
    LEditor := NyxTestEditorExchange(LBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LSession.ProjectSnapshot))),
      NyxField('selection', NyxData(LSession.SelectedID)), NyxField('view', NyxData(LSession.ActiveViewID))]));
    NyxTestEditorExchange(LBase, '/api/agents', LEditor.Field('token').AsText,
      NyxObject([NyxField('op', NyxData('commit')),
        NyxField('expectedRevision', LEditor.Field('state').Field('session').Field('revision')),
        NyxField('project', NyxData(EncodeNyxProject(LSession.ProjectSnapshot))),
        NyxField('selection', NyxData(LSession.SelectedID)), NyxField('view', NyxData(LSession.ActiveViewID))]));
    LClient := TNyxMCPTestClient.Create(ParamStr(2));
    LProcess := TProcess.Create(nil);
    LProcess.Executable := GetEnvironmentVariable('ProgramFiles(x86)') + '\Microsoft\Edge\Application\msedge.exe';
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    LProcess.Parameters.Add('--headless=new');
    LProcess.Parameters.Add('--disable-gpu');
    LProcess.Parameters.Add('--no-first-run');
    LProcess.Parameters.Add('--no-default-browser-check');
    LProcess.Parameters.Add('--disable-extensions');
    LProcess.Parameters.Add('--user-data-dir=' + LDirectory + 'profile');
    LProcess.Parameters.Add('--window-size=' + ParamStr(4) + ',1000');
    LProcess.Parameters.Add('--virtual-time-budget=150000');
    LProcess.Parameters.Add('--dump-dom');
    LProcess.Parameters.Add('--screenshot=' + LDirectory + 'observer.png');
    if ParamStr(4) = '390' then
    begin
      LProcess.Parameters.Add(LBase + '/agent-observer.html?host=1');
    end
    else
    begin
      LProcess.Parameters.Add(LBase + '/agent-observer.html');
    end;
    LProcess.Execute;
    LStarted := GetTickCount64;
    repeat
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
        end;
        LOutput := LOutput + LChunk;
      end;

      if GetTickCount64 - LStarted > 60000 then
      begin
        LProcess.Terminate(1);
        raise Exception.Create('Live observer exceeded its 60-second budget in phase ' + IntToStr(LPhase));
      end;

      if LPhase < 4 then
      begin
        LSummary := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
        LRevision := LSummary.Field('revision').AsInteger;
        case LPhase of
          0:
            begin

              if LSummary.Field('title').AsText = 'Agent observer ready' then
              begin
                LValue := LClient.Tool('nyx_transaction', Transaction('observer-live',
                  '[{"op":"create","kind":"badge","id":"observer-badge","parent":"home","properties":{"text":"Live 🌙漢字"}}]'));
                Check(not LValue.Field('isError').AsBoolean, 'MCP edit while browser observer is open');
                LPhase := 1;
                WriteLn('Observer native phase 1');
                Flush(Output);
              end;
            end;
          1:
            begin

              if LSummary.Field('permission').AsText = 'readOnly' then
              begin
                LValue := LClient.Tool('nyx_transaction', Transaction('observer-denied',
                  '[{"op":"title","value":"must not change"}]'));
                Check(LValue.Field('isError').AsBoolean, 'Real operator read-only action denies agent mutation');
                LPhase := 2;
                WriteLn('Observer native phase 2');
                Flush(Output);
              end;
            end;
          2:
            begin

              if LSummary.Field('permission').AsText = 'edit' then
              begin
                LValue := LClient.Tool('nyx_transaction', Transaction('observer-resume',
                  '[{"op":"title","value":"Agent observer changed"}]'));
                Check(not LValue.Field('isError').AsBoolean, 'Real operator re-enables same agent session');
                LPhase := 3;
                WriteLn('Observer native phase 3');
                Flush(Output);
              end;
            end;
          3:
            begin

              if LSummary.Field('pendingDraft').AsBoolean then
              begin
                LValue := LClient.Tool('nyx_transaction', Transaction('observer-draft-denied',
                  '[{"op":"title","value":"must preserve draft"}]'));
                Check(LValue.Field('isError').AsBoolean, 'Live source draft protects whole accepted pair');
                LPhase := 4;
                WriteLn('Observer native phase 4');
                Flush(Output);
              end;
            end;
        end;
      end
      else if LPhase = 4 then
      begin
        LSummary := LClient.Tool('nyx_session', NyxObject([])).Field('structuredContent');
        LRevision := LSummary.Field('revision').AsInteger;

        if not LSummary.Field('pendingDraft').AsBoolean then
        begin
          LValue := LClient.Tool('nyx_transaction', Transaction('observer-finish',
            '[{"op":"update","id":"observer-badge","properties":{"text":"Agent observer finished"}}]'));
          Check(not LValue.Field('isError').AsBoolean, 'Draft restore permits the next semantic edit');
          LPhase := 5;
        end;
      end;

      if LProcess.Running then
      begin
        Sleep(50);
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);
    LLog := TFileStream.Create(LDirectory + 'observer.dom.html', fmCreate);
    try
      LLog.WriteBuffer(LOutput[1], Length(LOutput));
    finally
      LLog.Free;
    end;
    Check(Pos('data-nyx-agent-observer="passed"', LOutput) > 0, 'Actual browser observer completed every phase');
    Check(LPhase = 5, 'Native agent completed semantic mutations');
    LClient.Close;
    LClient.Free;
    LClient := nil;
    LSession.Free;
    LSession := nil;
    LProcess.Free;
    LProcess := nil;
    WriteLn('PASS ', LCount, ' native agent/observer checks; browser evidence in ', LDirectory);
  except
    on LException: Exception do
    begin

      if LProcess <> nil then
      begin

        if LProcess.Running then
        begin
          LProcess.Terminate(1);
        end;
        LProcess.Free;
      end;
      LClient.Free;
      LSession.Free;
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
