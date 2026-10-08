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


unit nyx.test.resource.runtime;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Native protocol qualification; no listener, browser, child process, enrollment
  configuration or active user pair is changed. The common application journey
  separately exercises this wire against actual target controls. }
function RunNyxResourceRuntimeProtocol: Integer;

implementation

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.resources, nyx.application.resources, nyx.scheduler,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.resourceruns,
  nyx.studio.workspaces, nyx.studio.preview;

type
  TProtocolProbe = class
  public
    Core: TNyxAgentSession;
    Other: TNyxAgentSession;
    Tick: QWord;
    function Now: QWord;
    function Lookup(const AWorkspace: TNyxWorkspaceRef): TNyxAgentSession;
  end;

function TProtocolProbe.Now: QWord;
begin
  Result := Tick;
end;

function TProtocolProbe.Lookup(const AWorkspace: TNyxWorkspaceRef): TNyxAgentSession;
begin
  Result := Core;

  if AWorkspace.ID <> '' then
  begin
    Result := Other;
  end;
end;

function RunNyxResourceRuntimeProtocol: Integer;
var
  LProbe: TProtocolProbe;
  LBroker: TNyxResourceRuntimeBroker;
  LDocument: TNyxDocument;
  LScheduler: INyxScheduler;
  LResources: INyxApplicationResources;
  LSnapshot: INyxResourceRuntimeSnapshot;
  LPair: TNyxProjectPair;
  LGrant: TNyxDataValue;
  LSecond: TNyxDataValue;
  LWire: TNyxDataValue;
  LReply: TNyxDataValue;
  LBefore: TNyxDataValue;
  LToken: TNyxText;
  LRun: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Runtime protocol: ' + AReason);
    end;
    Inc(Result);
  end;
  function BuildData: TNyxDataValue;
  begin
    { This is trusted broker input, not evidence that a compiler executed. The
      ordinary host obtains these values from its admitted immutable job. }
    Result := NyxObject([NyxField('job', NyxData('01234567-0123-4567-89ab-0123456789ab')),
      NyxField('state', NyxData('succeeded')), NyxField('currentSource', NyxData(True)),
      NyxField('currentOutput', NyxData(True)), NyxField('target', NyxData('browser')),
      NyxField('scope', NyxData('application')), NyxField('view', NyxData(''))]);
  end;
  function Request(ASequence: Integer; const AOperation: TNyxText): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, 3);
    LFields[0] := NyxField('version', NyxData(1));
    LFields[1] := NyxField('operation', NyxData(AOperation));
    LFields[2] := NyxField('sequence', NyxData(ASequence));

    if AOperation = 'publish' then
    begin
      SetLength(LFields, 4);
      LFields[3] := NyxField('snapshot', LWire);
    end;
    Result := NyxObject(LFields);
  end;
  function Reports: TNyxDataValue;
  begin
    Result := LProbe.Core.Call('nyx_resources', 'protocol',
      NyxObject([NyxField('mode', NyxData('runtimes')),
      NyxField('expectedRevision', NyxData(LProbe.Core.Revision))])).Field('items');
  end;
  procedure RejectExchange(ASequence: Integer; const AToken: TNyxText;
    ACore: TNyxAgentSession; const AReason: TNyxText);
  var
    LRefused: Boolean;
  begin
    LRefused := False;
    try
      LBroker.Exchange(AToken, ACore, Request(ASequence, 'publish'));
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason);
  end;
begin
  Result := 0;
  LProbe := TProtocolProbe.Create;
  LBroker := nil;
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxColumn('home').Node);
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxTextResource('Tomorrow 🌙'));
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LProbe.Core := TNyxAgentSession.Create(LPair);
    LProbe.Other := TNyxAgentSession.Create;
    LProbe.Tick := 1000;
    LBroker := TNyxResourceRuntimeBroker.Create(LProbe.Now);
    LScheduler := NewNyxScheduler;
    LResources := NewNyxApplicationResources(LDocument.Resources, LScheduler,
      NyxApplicationResourceOptions);
    LSnapshot := NyxApplicationResourceDiagnostics(LResources).CaptureRuntime;
    LWire := EncodeNyxResourceRuntime(LSnapshot);
    Check(DecodeNyxResourceRuntime(LWire, LDocument.Resources).Page(0, 1).ToJSON =
      LSnapshot.Page(0, 1).ToJSON, 'initial authored values round-trip without inventing a published load');
    LGrant := LBroker.Issue(LProbe.Core, NyxPrimaryWorkspace, LPair, BuildData);
    LToken := LGrant.Field('token').AsText;
    LRun := LGrant.Field('run').AsText;
    Check((Length(LToken) = 76) and (Length(LRun) = 77), 'launch identity contains exact job and distinct run');
    Check(Pos(LToken, Reports.ToJSON) = 0, 'pending private capabilities never enter semantic reports');
    Check(Copy(NyxStudioRuntimeFragment(LGrant), 1, 13) = '#nyx-runtime=',
      'launch fragment separates private context from artifact HTTP paths');
    RejectExchange(1, '', LProbe.Core, 'missing producer capability refuses');
    RejectExchange(1, LToken, LProbe.Other, 'a different accepted pair cannot publish');
    RejectExchange(2, LToken, LProbe.Core, 'skipped initial sequence refuses');
    LReply := LBroker.Exchange(LToken, LProbe.Core, Request(1, 'publish'));
    Check(LReply.Field('active').AsBoolean and (Reports.Count = 1), 'first report enrolls one exact application');
    LBefore := Reports.Item(0).Field('sequence');
    Check(LBroker.Exchange(LToken, LProbe.Core, Request(1, 'publish')).ToJSON = LReply.ToJSON,
      'exact delivery retry returns its prior receipt');
    Check(Reports.Item(0).Field('sequence').ToJSON = LBefore.ToJSON,
      'delivery retry never republishes or increments runtime sequence');
    LBroker.Exchange(LToken, LProbe.Core, Request(2, 'heartbeat'));
    Check(Reports.Item(0).Field('sequence').ToJSON = LBefore.ToJSON,
      'unchanged heartbeat extends liveness without a fake resource change');
    RejectExchange(1, LToken, LProbe.Core, 'stale publisher sequence refuses');
    LProbe.Tick := 60001;
    LBroker.Expire(LProbe.Lookup);
    Check(Reports.Item(0).Field('active').AsBoolean, 'accepted heartbeat refreshes the deadline');
    LProbe.Tick := 61001;
    LBroker.Expire(LProbe.Lookup);
    Check(not Reports.Item(0).Field('active').AsBoolean, 'expiry visibly retires accepted evidence');
    RejectExchange(3, LToken, LProbe.Core, 'expired producer authority is revoked');
    LGrant := LBroker.Issue(LProbe.Core, NyxPrimaryWorkspace, LPair, BuildData);
    LToken := LGrant.Field('token').AsText;
    LBroker.Exchange(LToken, LProbe.Core, Request(1, 'publish'));
    LReply := LBroker.Exchange(LToken, LProbe.Core, Request(2, 'retire'));
    Check(not LReply.Field('active').AsBoolean, 'explicit producer retirement is acknowledged');
    LBroker.Expire(LProbe.Lookup);
    Check(LBroker.Exchange(LToken, LProbe.Core, Request(2, 'retire')).ToJSON = LReply.ToJSON,
      'retirement tombstone permits an exact delivery retry');
    RejectExchange(3, LToken, LProbe.Core, 'retired producer cannot publish again');
    LGrant := LBroker.Issue(LProbe.Core, NyxPrimaryWorkspace, LPair, BuildData);
    for LIndex := 1 to 7 do
    begin
      LSecond := LBroker.Issue(LProbe.Core, NyxPrimaryWorkspace, LPair, BuildData);
    end;
    LRejected := False;
    try
      LBroker.Issue(LProbe.Core, NyxPrimaryWorkspace, LPair, BuildData);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'eight live per-project launch grants bound membership');
    LWire := NyxObject([NyxField('version', NyxData(1)), NyxField('stopped', NyxData(False)),
      NyxField('locale', NyxData('')), NyxField('fallback', NyxData('')),
      NyxField('entries', NyxArray([]))]);
    RejectExchange(1, LGrant.Field('token').AsText, LProbe.Core,
      'missing complete variant membership refuses before enrollment');
    LResources.Stop;
    LWire := EncodeNyxResourceRuntime(NyxApplicationResourceDiagnostics(LResources).CaptureRuntime);
    LBroker.Exchange(LGrant.Field('token').AsText, LProbe.Core, Request(1, 'publish'));
    Check(not Reports.Item(Reports.Count - 1).Field('active').AsBoolean,
      'a stopped final snapshot revokes publication without a fake live host');
  finally
    LBroker.Free;
    LProbe.Core.Free;
    LProbe.Other.Free;
    LProbe.Free;
    LDocument.Free;
    LSnapshot := nil;
    LResources := nil;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;
  end;
end;

end.
