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

program nyx_file_open_application_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec,
  nyx.state, nyx.logic, nyx.flow, nyx.flow.types, nyx.files.open, nyx.scheduler,
  nyx.test.flow.conditionfixture
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, nyx.widgets.lcl, nyx.application.lcl{$endif};

type
  { Explicit substituted host for application-authority composition only. The
    immutable snapshot is data, with no disk path or OS permission. Real file
    selection/read is qualified separately by nyx_file_open_controls. }
  TReadHost = class(TInterfacedObject, INyxFileOpenHost)
  public
    Calls: Integer;
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

  { Application-supplied resolver counts non-file operations. Its deliberate
    ordinary fallback would construct an unprivileged open operation; only the
    separately configured file layer may supply the read host. }
  TCreatorResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Calls: Integer;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

var
  GApp: {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};
  GDoc: TNyxDocument;
  GHost: TReadHost;
  GHostLease: INyxFileOpenHost;
  GResolver: TCreatorResolver;
  GResolverLease: INyxLogicResolver;
  GStage: Integer;
  GPass: Integer;
  GChecks: Integer;
  GBefore: TNyxText;
  GStarted: Double;
  {$ifdef PAS2JS}GMount: TJSHTMLElement;{$endif}
  {$ifndef PAS2JS}
  GSchedulers: array of INyxScheduler;
  {$endif}

function TReadHost.BeginOpen(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply): INyxFileOpenRequest;
var
  LBytes: TNyxBytes;
begin
  AScheduler.RequireUI;
  Inc(Calls);
  LBytes := NyxEncodeUTF8('{"title":"An independent report","count":9007199254740993}');
  Result := nil;
  AReply(nfooRead, NyxFileSnapshot(NyxFileMetadata('report.json', Length(LBytes)), LBytes), '');
end;

function TCreatorResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin
  Inc(Calls);
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Retire;
{$ifndef PAS2JS}
var
  LCount: Integer;
{$endif}
begin

  if GApp <> nil then
  begin
    {$ifndef PAS2JS}
    LCount := Length(GSchedulers);
    SetLength(GSchedulers, LCount + 1);
    GSchedulers[LCount] := GApp.Context.Scheduler;
    {$endif}
    FreeAndNil(GApp);
  end;
  GResolverLease := nil;
  GHostLease := nil;
  {$ifdef PAS2JS}

  if GMount <> nil then
  begin
    GMount.remove;
    GMount := nil;
  end;
  {$endif}
end;

procedure BeginPass;
var
  LRefused: Boolean;
begin
  GHost := TReadHost.Create;
  GHostLease := GHost;
  GResolver := TCreatorResolver.Create;
  GResolverLease := GResolver;
  GApp := {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif}.Create;

  if GPass = 0 then
  begin
    GApp.ConfigureFileOpening(GHostLease).ConfigureLogic(GResolverLease).ConfigureFileOpening(nil);
  end
  else if GPass = 1 then
  begin
    GApp.ConfigureFileOpening(GHostLease).ConfigureLogic(GResolverLease);
  end
  else
  begin
    GApp.ConfigureLogic(GResolverLease).ConfigureFileOpening(GHostLease);
  end;
  {$ifdef PAS2JS}
  GMount := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GMount);
  GApp.Run(GDoc, GMount);
  {$else}
  GApp.Mount(GDoc);
  GApp.Window.Show;
  {$endif}
  LRefused := False;
  try
    GApp.ConfigureFileOpening(nil);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'mounted application refuses authority mutation without replacing its read host');
  {$ifdef PAS2JS}
  GApp.View.ElementFor('open-report').click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('open-report')).Click;
  {$endif}
  GStarted := Clock;
end;

function Step: Boolean;
var
  LReport: TNyxDataValue;
begin
  Result := False;

  if GStage = 0 then
  begin
    BeginPass;
    GStage := 1;
    Exit;
  end;

  if Clock - GStarted > 15000 then
  begin
    raise Exception.Create('Application read-authority deadline');
  end;
  LReport := GApp.FlowRoutes.Report(0);

  if GPass = 0 then
  begin

    if LReport.Field('failure').AsText = '' then
    begin
      Exit;
    end;
    Check(GHost.Calls = 0, 'nil revocation prevents invoking the previously configured read host');
    Check(not GApp.State.GetValue(NyxBooleanState('read-complete')) and
      (GApp.State.GetValue(NyxTextState('pretty')) = 'Choose a report to begin.'),
      'unprivileged routes retain the old bound report without a successful prefix');
  end
  else
  begin

    if not GApp.State.GetValue(NyxBooleanState('read-complete')) then
    begin

      if LReport.Field('failure').AsText <> '' then
      begin
        raise Exception.Create('Configured read route failed: ' +
          String(LReport.Field('failure').AsText));
      end;
      Exit;
    end;
    Check(GHost.Calls = 1, 'explicit read host is invoked once in either configuration order');
    Check(GResolver.Calls = 5, 'non-file operation construction still reaches the creator resolver');
    Check((GApp.State.GetValue(NyxTextState('read-name')) = 'report.json') and
      (Pos('9007199254740993', GApp.State.GetValue(NyxTextState('pretty'))) > 0),
      'ordinary saved routes publish complete exact read data through the configured layer');
  end;
  Check(TNyxCodec.Encode(GDoc) = GBefore,
    'application configuration and runtime outcomes preserve the exact portable document');
  Retire;
  Inc(GPass);
  GStage := 0;
  Result := GPass = 3;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if Step then
    begin
      document.body.setAttribute('data-open-application', 'passed');
      document.body.setAttribute('data-open-application-checks', IntToStr(GChecks));
      GDoc.Free;
      GDoc := nil;
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-open-application', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      Retire;
      GDoc.Free;
      GDoc := nil;
    end;
  end;
end;
{$endif}

{$ifndef PAS2JS}
procedure Drain;
var
  LIndex: Integer;
  LStarted: Double;
  LMonitor: INyxSchedulerMonitor;
begin
  for LIndex := 0 to High(GSchedulers) do
  begin
    GSchedulers[LIndex].Shutdown;
    LMonitor := GSchedulers[LIndex] as INyxSchedulerMonitor;
    LStarted := Clock;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;
      Sleep(1);

      if Clock - LStarted > 5000 then
      begin
        raise Exception.Create('Owned read-authority scheduler failed to retire');
      end;
    until (Clock - LStarted >= 100) and (LMonitor.WorkerLoad.ActiveWorkers = 0);
    LMonitor := nil;
    GSchedulers[LIndex] := nil;
  end;
  GSchedulers := nil;
end;
{$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDoc := ConditionalFileDocument;
  GBefore := TNyxCodec.Encode(GDoc);
  {$ifdef PAS2JS}
  Drive;
  {$else}
  try
    while not Step do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Sleep(20);
    end;
    Drain;
    WriteLn('PASS ', GChecks, ' native application opening-authority checks with substituted host');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  Retire;
  Drain;
  GDoc.Free;
  {$endif}
end.
