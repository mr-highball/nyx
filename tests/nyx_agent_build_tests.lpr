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

program nyx_agent_build_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects, nyx.studio.builds,
  nyx.studio.compiler;

var
  GCount: Integer;
  GSession: TNyxAgentSession;
  GPair: TNyxProjectPair;
  GRevision: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

procedure Refuse(ARevision: Integer; AScope: TNyxBuildScope;
  const AView, AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    GSession.BuildPair(ARevision, AScope, AView);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
  Check(GSession.Revision = GRevision, 'Refused build retains document revision');
end;

var
  LPair: TNyxProjectPair;
  LValue: TNyxDataValue;
  LScope: TNyxBuildScope;
  LTarget: TNyxBuildTarget;
  LRefused: Boolean;
  LReport: INyxCompilerReport;
  LOrder: TNyxCompilerDiagnosticIndices;
begin
  GSession := nil;
  try
    GSession := TNyxAgentSession.Create;
    GRevision := GSession.Revision;
    GPair := GSession.BuildPair(GRevision, bsApplication, '');
    LPair := GSession.BuildPair(GRevision, bsView, 'home');
    Check(EncodeNyxProject(GPair) = EncodeNyxProject(LPair), 'All scopes capture the complete immutable accepted pair');
    Check(GSession.CurrentPair(GPair), 'Exact source/design currentness');
    LPair := GSession.BuildPair(GRevision, bsReusable, 'welcome-card');
    Check(EncodeNyxProject(LPair) = EncodeNyxProject(GPair), 'Reusable capture preserves full companion frame');
    Refuse(GRevision - 1, bsApplication, '', 'Stale revision refuses');
    Refuse(GRevision, bsApplication, 'home', 'Application refuses a view');
    Refuse(GRevision, bsView, 'welcome-card', 'Page scope refuses reusable root');
    Refuse(GRevision, bsReusable, 'home', 'Reusable scope refuses page root');
    Refuse(GRevision, bsView, 'missing', 'Missing page refuses');
    Refuse(GRevision, bsView, 'welcome-title', 'Descendant is not a page root');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Refuse(GRevision, bsView, 'home', 'Read-only cannot launch compilers');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('disabled'))]));
    Refuse(GRevision, bsApplication, '', 'Disabled cannot launch compilers');
    GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    LValue := GSession.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('operationId', NyxData('build-currentness')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('title')), NyxField('value', NyxData('A new application'))])]))]));
    GRevision := LValue.Field('revision').AsInteger;
    Check(not GSession.CurrentPair(GPair), 'Changed design makes the earlier job stale');
    GSession.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('operationId', NyxData('build-currentness-undo')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('undo'))]));
    GRevision := GSession.Revision;
    Check(GSession.CurrentPair(GPair), 'Undo restores exact pair independently of monotonic revision');
    LPair := GPair;
    LPair.Pending := True;
    LPair.Draft := GPair.Source + #10 + '// Unaccepted draft';
    LPair.DraftBase := GPair.Source;
    GSession.Exchange(NyxObject([
      NyxField('op', NyxData('commit')), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    GRevision := GSession.Revision;
    Refuse(GRevision, bsView, 'home', 'Pending draft refuses a build');
    Check(not GSession.CurrentPair(GPair), 'Pending draft disables current job diagnostics');
    for LScope := Low(TNyxBuildScope) to High(TNyxBuildScope) do
    begin
      Check(ParseNyxBuildScope(NyxBuildScopeName(LScope)) = LScope, 'Closed scope enum round trip');
    end;
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      Check(ParseNyxBuildTarget(NyxBuildTargetName(LTarget)) = LTarget, 'Closed target enum round trip');
    end;
    LRefused := False;
    try
      ParseNyxBuildTarget('shell');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Unpublished compiler target refuses');
    LReport := ReadNyxCompilerReport('unit fixture;', 'unit fixture;', 'fixture.pas',
      'fixture.pas(1,1) Warning: first warning' + #10 +
      'fixture.pas(1,1) Note: first note' + #10 +
      'fixture.pas(1,1) Error: actionable error' + #10 +
      'fixture.pas(1,1) Fatal: fatal error' + #10 +
      'fixture.pas(1,1) Warning: second warning');
    LOrder := NyxCompilerDiagnosticOrder(LReport);
    Check((Length(LOrder) = 5) and (LOrder[0] = 2) and (LOrder[1] = 3) and
      (LOrder[2] = 0) and (LOrder[3] = 4) and (LOrder[4] = 1),
      'Bounded presentation prioritizes errors and preserves order within each severity');
    Check(LReport.Item(0).Severity = csWarning, 'Presentation does not mutate the immutable compiler report');
    Check(Length(NyxCompilerDiagnosticOrder(nil)) = 0, 'Absent report has no diagnostic indices');
    LReport := nil;
    GSession.Free;
    GSession := nil;
    WriteLn('PASS ', GCount, ' portable build admission/currentness checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-agent-build-checks', IntToStr(GCount));
    document.body.setAttribute('data-nyx-agent-build-ready', 'passed');
    {$endif}
  except
    on LException: Exception do
    begin
      GSession.Free;
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-agent-build-error', LException.Message);
      {$else}
      Halt(1);
      {$endif}
    end;
  end;
end.
