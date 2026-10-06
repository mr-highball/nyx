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

program nyx_build_job_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.studio.buildjobs,
  nyx.studio.agents, nyx.studio.projects, nyx.studio.builds,
  nyx.studio.outputs, nyx.studio.compiler;

var
  GJobs: TNyxBuildJobs;
  GSession: TNyxAgentSession;
  GPair: TNyxProjectPair;
  GOutput: TNyxOutputConfiguration;
  GCount: Integer;
  GOutputID: TNyxText;
  GRoot: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

function Args(const AID: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GSession.Revision)),
    NyxField('operationId', NyxData(AID)), NyxField('outputID', NyxData(GOutputID)),
    NyxField('target', NyxData('browser')), NyxField('scope', NyxData('view')),
    NyxField('view', NyxData('home'))]);
end;

function Status(const AID: TNyxText): TNyxDataValue;
var
  LPair: TNyxProjectPair;
  LCurrent: Boolean;
begin
  Result := GJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
    NyxField('job', NyxData(AID))]), LPair, LCurrent);
  Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair), 'Job retains independent immutable text');
end;

procedure Wait(const AID: TNyxText);
var
  LStarted: QWord;
  LPair: TNyxProjectPair;
  LCurrent: Boolean;
  LValue: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    LValue := GJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(AID))]), LPair, LCurrent);

    if NyxBuildJobTerminal(ParseNyxBuildJobState(LValue.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Owned compiler resource fixture timed out');
    end;
    Sleep(20);
  until False;
  Check(LValue.Field('state').AsText = 'succeeded', 'Resource fixture completes: ' + LValue.ToJSON);
end;

var
  LFirstArgs: TNyxDataValue;
  LFirst: TNyxDataValue;
  LSecond: TNyxDataValue;
  LThird: TNyxDataValue;
  LValue: TNyxDataValue;
  LRetry: TNyxDataValue;
  LIndex: Integer;
  LRefused: Boolean;
  LActor: TNyxText;
  LOutcome: TNyxText;
  LPair: TNyxProjectPair;
  LReport: INyxCompilerReport;
  LCurrent: Boolean;
  LCount: Integer;
  LProfile: TNyxText;
begin
  GJobs := nil;
  GSession := nil;
  GOutput := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply owned scratch root, fixture compiler executable and runtime file');
    end;
    GRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
    GOutput := TNyxOutputConfiguration.Create;
    GOutput.SetField('pas2js', ExpandFileName(ParamStr(2)));
    GOutput.SetField('runtime', ExpandFileName(ParamStr(3)));
    GSession := TNyxAgentSession.Create;
    GPair := GSession.BuildPair(GSession.Revision, bsView, 'home');
    GJobs := TNyxBuildJobs.Create(GRoot, GOutput.Encode);
    GOutputID := GJobs.Outputs.Field('outputID').AsText;
    LFirstArgs := Args('first');
    GJobs.AdmitRequest(LFirstArgs);
    LFirst := GJobs.Submit('Scooty', LFirstArgs, GPair);
    LSecond := GJobs.Submit('Scooty', Args('second'), GPair);
    LThird := GJobs.Submit('Scooty', Args('third-queued'), GPair);
    Check(LThird.Field('state').AsText = 'queued', 'Third job admits to the bounded queue');
    Check(GJobs.Retry('Scooty', LFirstArgs, LRetry) and
      (LRetry.ToJSON = LFirst.ToJSON), 'Running retry does not create a third worker');
    LProfile := GOutput.Encode;
    GOutput.SetField('pas2js', '');
    GJobs.Configure(GOutput.Encode);
    Check(not GJobs.Outputs.Field('outputs').Item(0).Field('ready').AsBoolean,
      'Future readiness reflects the operator profile');
    Wait(LFirst.Field('job').AsText);
    Wait(LSecond.Field('job').AsText);
    Wait(LThird.Field('job').AsText);
    LValue := GJobs.Status(NyxObject([NyxField('mode', NyxData('status')),
      NyxField('job', LFirst.Field('job'))]), LPair, LCurrent);
    Check(not LCurrent and (LValue.Field('outputID').AsText = GOutputID),
      'Running job retains its captured ready profile after configuration changes');
    LRefused := False;
    try
      GJobs.Submit('Scooty', Args('old-output'), GPair);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Old output identity refuses a new request');
    Check(GJobs.Retry('Scooty', LFirstArgs, LRetry), 'Exact old-profile retry returns its existing receipt');
    GJobs.Configure(LProfile);
    LCount := 0;
    while GJobs.TakeCompletion(LActor, LOutcome, LPair, LReport) do
    begin
      Inc(LCount);
      Check((LActor = 'Scooty') and (LReport <> nil), 'Completion transfers retained report without worker borrowing');
    end;
    Check(LCount = 3, 'Three completion notifications drain exactly once');
    for LIndex := 2 to 16 do
    begin
      LValue := GJobs.Submit('Scooty', Args('retained-' + IntToStr(LIndex)), GPair);
      Wait(LValue.Field('job').AsText);
      while GJobs.TakeCompletion(LActor, LOutcome, LPair, LReport) do
      begin
        Check(True, 'Terminal notification drains before retention eviction');
      end;
    end;
    LRefused := False;
    try
      Status(LFirst.Field('job').AsText);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Oldest terminal handle expires at the sixteen-job boundary');
    Check(GJobs.Retry('Scooty', LFirstArgs, LRetry) and
      (LRetry.ToJSON = LFirst.ToJSON), 'Expired handle retry returns original receipt without rebuilding');
    Check(not GJobs.Retry('Other actor', LFirstArgs, LRetry), 'Retry identity is actor scoped');
    LRefused := False;
    try
      GJobs.Retry('Scooty', NyxObject([
        NyxField('mode', NyxData('request')), NyxField('operationId', NyxData('first'))]), LRetry);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Changed exact retry bytes refuse');
    { Destroy while the final worker is active. The owned join must finish before
      releasing its pair/profile/guard, without callbacks into the model. }
    GJobs.Submit('Scooty', Args('join-on-close'), GPair);
    FreeAndNil(GJobs);
    FreeAndNil(GSession);
    FreeAndNil(GOutput);
    LReport := nil;
    WriteLn('PASS ', GCount, ' native compiler job resource/lifetime checks');
  except
    on LException: Exception do
    begin
      GJobs.Free;
      GSession.Free;
      GOutput.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
