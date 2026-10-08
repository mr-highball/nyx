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
program nyx_clock_authoring_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpclient, nyx.text, nyx.data, nyx.types,
  nyx.model, nyx.codec, nyx.composition, nyx.source,
  nyx.times, nyx.contract, nyx.root.types, nyx.studio.edits, nyx.studio.builds,
  nyx.studio.projects,
  nyx.test.mcp.client, nyx.test.browser.pipe;

var
  GClient: TNyxMCPTestClient;
  GStudio: TNyxBrowserPipe;
  GRendered: TNyxBrowserPipe;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GAcceptedSource: TNyxText;
  GAcceptedPair: TNyxProjectPair;
  GBase: TNyxText;
  GDirectory: String;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Clock review: ' + AReason);
  end;
  Inc(GChecks);
end;

{ One live authenticated transport owns every review operation. The optional
  unscoped route is used only for lifecycle and read-only primary preservation.
  Explicit refusals are evidence, never permission to retry another context. }
function Call(const ATool: TNyxText; const AFields: array of TNyxDataField;
  AScoped: Boolean = True; ARefuse: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LReply: TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LFields, Length(AFields) + Ord(AScoped));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := AFields[LIndex];
  end;

  if AScoped then
  begin
    Check(GReview <> '', 'an owned review is required before a scoped call');
    LFields[High(LFields)] := NyxField('review', NyxData(GReview));
  end;
  LReply := GClient.Tool(ATool, NyxObject(LFields));
  Check(LReply.Field('isError').AsBoolean = ARefuse,
    ATool + ' unexpected admission: ' + Copy(LReply.ToJSON, 1, 700));
  Result := LReply.Field('structuredContent');
end;

{ Only fresh owned output directories are admitted before connecting. UTF-8
  bytes preserve exact source without ANSI collections or global codepage changes.
  Receipts containing local preview/build grants remain private build artifacts. }
procedure Save(const AName: String; const AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Source: TNyxText;
var
  LReply: TNyxDataValue;
  LLines: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
begin
  Result := '';
  LLine := 1;
  repeat
    LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)),
      NyxField('count', NyxData(80))]);
    Check(LReply.Field('revision').AsInteger = GRevision,
      'bounded Pascal windows stay at one exact revision');
    LLines := LReply.Field('lines');
    Check(LLines.Count > 0, 'the bounded source window makes progress');
    for LIndex := 0 to LLines.Count - 1 do
    begin
      Result := Result + LLines.Item(LIndex).AsText + TNyxText(#10);
    end;
    Inc(LLine, LLines.Count);
  until LLine > LReply.Field('totalLines').AsInteger;
end;

function Domain: TNyxDataValue;
begin
  Result := Call('nyx_node', [NyxField('id', NyxData('meeting-start')),
    NyxField('keys', NyxArray([NyxData('value')])), NyxField('limit', NyxData(1)),
    NyxField('valueDomain', NyxData(True)), NyxField('domainLimit', NyxData(1))]);
  Check(Result.Field('revision').AsInteger = GRevision,
    'the paged clock context belongs to the current revision');
  Result := Result.Field('valueDomain');
end;

procedure Transaction(const AOperation: TNyxText;
  const AOperations: array of TNyxDataValue; ARefuse: Boolean = False);
begin
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AOperation)),
    NyxField('operations', NyxArray(AOperations))], True, ARefuse);

  if not ARefuse then
  begin
    Inc(GRevision);
  end;
end;

procedure History(const ADirection: TNyxText);
begin
  Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('clock-' + ADirection))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

{ A normal observing Studio is read through bounded DOM queries only for visible
  activity. All authored document operations stay semantic. Its isolated browser
  profile does not touch an existing user's tab, recovery or viewport settings. }
procedure Observe;
var
  LIndex: Integer;
  LStarted: QWord;
  LLabel: TNyxText;
  LOwner: TNyxText;
  LBox: TNyxBrowserBox;
begin
  LStarted := GetTickCount64;
  repeat
    for LIndex := 0 to 7 do
    begin
      LLabel := GStudio.ElementHTML('[data-node="studio-agent-review-label-' +
        IntToStr(LIndex) + '"]');

      if Pos('Clock workshop', LLabel) > 0 then
      begin
        LOwner := GStudio.ElementHTML('[data-node="studio-agent-review-owner-' +
          IntToStr(LIndex) + '"]');

        if Pos('revision ' + IntToStr(GRevision), LOwner) > 0 then
        begin
          GStudio.Reveal('[data-node="studio-agent-review-' + IntToStr(LIndex) + '"]');
          LBox := GStudio.Bounds('[data-node="studio-agent-review-owner-' +
            IntToStr(LIndex) + '"]');
          Check((LBox.Height > 0) and (LBox.Top >= 0) and
            (LBox.Top + LBox.Height <= 900),
            'the owned review activity is physically inside the observing viewport');
          Check(True, 'ordinary Studio visibly observes this semantic review revision');
          Exit;
        end;
      end;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      GStudio.Capture('observation-timeout');
      raise Exception.Create('Ordinary Studio did not observe the owned clock review');
    end;
    Sleep(50);
  until False;
end;

function Fetch(const APath: TNyxText): TNyxText;
var
  LHTTP: TFPHTTPClient;
  LBytes: TMemoryStream;
begin
  LHTTP := TFPHTTPClient.Create(nil);
  LBytes := TMemoryStream.Create;
  try
    LHTTP.Get(GBase + '/' + APath, LBytes);
    SetLength(Result, LBytes.Size);

    if LBytes.Size <> 0 then
    begin
      Move(LBytes.Memory^, Result[1], LBytes.Size);
    end;
  finally
    LBytes.Free;
    LHTTP.Free;
  end;
end;

{ The small owned review's immutable semantic preview establishes exact design
  bytes beside the bounded source windows for paired history checks. It is used
  selectively for this invariant, not to understand or author the editor through
  a full state dump. No screenshot or arbitrary URL is needed for that read. }
function Snapshot: TNyxText;
var
  LReply: TNyxDataValue;
  LPacket: TNyxDataValue;
  LPath: TNyxText;
begin
  LReply := Call('nyx_preview', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('view', NyxData('home')), NyxField('width', NyxData(1100)),
    NyxField('height', NyxData(900))]);
  LPath := StringReplace(LReply.Field('url').AsText,
    GBase + '/agent-preview.html?', 'api/agents/preview?', []);
  Check(Pos('api/agents/preview?', LPath) = 1,
    'the semantic preview belongs to the configured loopback editor');
  LPacket := TNyxDataValue.ParseJSON(Fetch(LPath));
  Check(LPacket.Field('revision').AsInteger = GRevision,
    'the immutable design snapshot belongs to the exact source revision');
  Result := EncodeNyxProject(NyxProjectPair(LPacket.Field('design').AsText, Source));
end;

{ Real compiler jobs consume the immutable accepted pair. Poll the same admitted
  handle until its terminal lifecycle state; timeout never means cancellation or
  permission to resubmit. Target/scope are closed Pascal enums at this boundary. }
procedure Build(ATarget: TNyxBuildTarget; AScope: TNyxBuildScope;
  const AOutput: TNyxText);
var
  LFields: array of TNyxDataField;
  LReply: TNyxDataValue;
  LJob: TNyxText;
  LStem: TNyxText;
  LStarted: QWord;
  LValue: TNyxText;
  LDocument: TNyxDocument;
  LScopedDocument: TNyxDocument;
  LExpectedSource: TNyxText;
  LCompiledSource: TNyxText;
begin
  LStem := NyxBuildTargetName(ATarget) + '-' + NyxBuildScopeName(AScope);
  SetLength(LFields, 6 + Ord(AScope <> bsApplication));
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[2] := NyxField('operationId', NyxData('clock-' + LStem));
  LFields[3] := NyxField('outputID', NyxData(AOutput));
  LFields[4] := NyxField('target', NyxData(NyxBuildTargetName(ATarget)));
  LFields[5] := NyxField('scope', NyxData(NyxBuildScopeName(AScope)));

  if AScope <> bsApplication then
  begin

    if AScope = bsReusable then
    begin
      LValue := 'appointment';
    end
    else
    begin
      LValue := 'home';
    end;
    LFields[6] := NyxField('view', NyxData(LValue));
  end;
  LReply := Call('nyx_build', LFields);
  Save('build-' + String(LStem) + '-admitted.json', LReply.ToJSON);
  LJob := LReply.Field('job').AsText;
  Check(Call('nyx_build', LFields).Field('job').AsText = LJob,
    'an exact compiler retry returns the same admitted handle');
  LStarted := GetTickCount64;
  repeat
    LReply := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(4))]);

    if GetTickCount64 - LStarted > 90000 then
    begin
      Save('build-' + String(LStem) + '-still-live.json', LReply.ToJSON);
      WriteLn('Clock compiler still nonterminal / ', LStem, ' / same admitted handle');
      Flush(Output);
      LStarted := GetTickCount64;
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LReply.Field('state').AsText));
  Save('build-' + String(LStem) + '.json', LReply.ToJSON);
  Check((LReply.Field('state').AsText = 'succeeded') and
    LReply.Field('currentSource').AsBoolean,
    'the actual clock compiler succeeds at its exact accepted revision');
  LCompiledSource := Fetch(LReply.Field('compiledSource').AsText);
  Save('compiled-' + String(LStem) + '.pas', LCompiledSource);
  LExpectedSource := GAcceptedSource;

  if AScope <> bsApplication then
  begin
    { Page/reusable compilation deliberately replaces only the managed views
      with the requested root and reachable definitions. It must not compile
      unrelated pages. Verify the actual frozen-service bytes against the public
      owned projection/companion contract, preserving helpers and exact policy. }
    LDocument := TNyxCodec.Decode(GAcceptedPair.Design);
    LScopedDocument := nil;
    try
      LScopedDocument := CloneNyxViewDocument(LDocument,
        LDocument.Find(LFields[6].Value.AsText));
      Check((LScopedDocument.Count = 1) and (LScopedDocument.Find('preparation') = nil),
        'a scoped build retains its requested root without the unrelated page');
      LExpectedSource := PrepareNyxCompanion(LDocument, LScopedDocument,
        GAcceptedSource, True);
    finally
      LScopedDocument.Free;
      LDocument.Free;
    end;
  end;
  Check(LCompiledSource = LExpectedSource,
    'actual HTTP compiler input equals the exact accepted application or owned view projection');

  if (ATarget = btBrowser) and (AScope = bsApplication) then
  begin
    GRendered := TNyxBrowserPipe.Create(String(GBase) + '/' +
      String(LReply.Field('artifact').AsText), GDirectory + 'compiled-browser', 1100, 900);
    LStarted := GetTickCount64;
    repeat

      if GRendered.TryFieldValue('[data-node="meeting-start"] input', LValue) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 30000 then
      begin
        GRendered.Capture('compiled-timeout');
        raise Exception.Create('Compiled clock application did not mount its real input');
      end;
      Sleep(50);
    until False;
    Check(TNyxClockTime.FromText(LValue).Compare(NyxTime(0, 30)) = 0,
      'the compiled actual browser input represents the exact authored clock reading');
    Check(GRendered.Exists('[data-node="home-appointment"]'),
      'the compiled application realizes its reusable appointment');
    GRendered.Capture('compiled-clock');
    FreeAndNil(GRendered);
  end;
  WriteLn('Qualified clock ', LStem);
  Flush(Output);
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LCreation: TNyxDataValue;
  LOperations: array of TNyxDataValue;
  LDomainBefore: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LOutput: TNyxText;
  LIndex: Integer;
  LTarget: TNyxBuildTarget;
  LScope: TNyxBuildScope;
begin
  LPrimary := Call('nyx_session', [], False);
  LReply := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
    NyxField('base', NyxData('empty')), NyxField('label', NyxData('Clock workshop')),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData('clock-review-create'))], False);
  GReview := LReply.Field('review').AsText;
  Save('review-created.json', LReply.ToJSON);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;

  { These strings are the explicit MCP document-operation wire boundary.
    Behavioral clock policies below use the public specialized Pascal builder. }
  LCreation := TNyxDataValue.ParseJSON(
    '[{"op":"title","value":"Meeting planner"},' +
    '{"op":"create","kind":"page","id":"home","root":"page","properties":{"padding":24,"gap":16}},' +
    '{"op":"create","kind":"heading","id":"schedule-heading","parent":"home","properties":{"text":"Plan a thoughtful day"}},' +
    '{"op":"create","kind":"time","id":"meeting-start","parent":"home","properties":{"text":"Starts at","value":"00:30:00.000"}},' +
    '{"op":"create","kind":"page","id":"preparation","root":"page","properties":{"padding":24,"gap":16}},' +
    '{"op":"create","kind":"heading","id":"preparation-heading","parent":"preparation","properties":{"text":"Prepare for your appointment"}},' +
    '{"op":"create","kind":"card","id":"appointment","root":"component","properties":{"padding":20,"gap":12}},' +
    '{"op":"create","kind":"heading","id":"appointment-heading","parent":"appointment","properties":{"text":"Your appointment","part":"heading"}},' +
    '{"op":"create","kind":"time","id":"appointment-time","parent":"appointment","properties":{"text":"Appointment time","value":"09:00","part":"time"}},' +
    '{"op":"instance","id":"home-appointment","component":"appointment","parent":"home"},' +
    '{"op":"instance","id":"preparation-appointment","component":"appointment","parent":"preparation"}]');
  SetLength(LOperations, LCreation.Count + 1);
  for LIndex := 0 to LCreation.Count - 1 do
  begin
    LOperations[LIndex] := LCreation.Item(LIndex);
  end;
  LOperations[LCreation.Count] := NyxSetValueDomain(NyxControl('meeting-start'),
    NyxTimeDomain.Range(NyxTime(23, 0).WithPrecision(ntpMillisecond),
      NyxTime(1, 0).WithPrecision(ntpMillisecond)).StepMilliseconds(1500)
      .Choices([NyxTime(23, 0), NyxTime(0, 30), NyxNoTime])).ToData;
  Transaction('clock-compose', LOperations);
  Call('nyx_select', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('clock-select-home')),
    NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  LBefore := Snapshot;
  LDomainBefore := Domain;
  Check((LDomainBefore.Field('format').AsText = 'time') and
    LDomainBefore.Field('crossesMidnight').AsBoolean and
    (LDomainBefore.Field('stepMilliseconds').AsInteger = 1500) and
    (LDomainBefore.Field('choices').Count = 1) and
    (LDomainBefore.Field('totalChoices').AsInteger = 3),
    'authenticated clock context preserves overnight stepping and pages exact choices');

  GStudio := TNyxBrowserPipe.Create(String(GBase) + '/', GDirectory + 'observing-studio', 1280, 900);
  GStudio.Click('[data-node="action-agents"]');
  Observe;
  Transaction('clock-policy', [NyxSetValueDomain(NyxControl('meeting-start'),
    NyxTimeDomain.Range(NyxTime(23, 0).WithPrecision(ntpMillisecond),
      NyxTime(1, 0).WithPrecision(ntpMillisecond)).StepMilliseconds(500)
      .Choices([NyxTime(23, 0), NyxTime(0, 30), NyxNoTime])).ToData]);
  LAfter := Snapshot;
  GAcceptedPair := DecodeNyxProject(LAfter);
  GAcceptedSource := GAcceptedPair.Source;
  Check((Pos('INyxTime', GAcceptedSource) > 0) and
    (Pos('NewNyxTime(', GAcceptedSource) > 0) and
    (Pos('NyxTimeDomain', GAcceptedSource) > 0) and
    (Pos('.StepMilliseconds(500)', GAcceptedSource) > 0),
    'semantic editing generates specialized managed and typed clock Pascal');
  Observe;
  GStudio.Capture('observing-clock');

  { Deliberately malformed external bytes qualify atomic group refusal. They are
    never an authoring shortcut for a construct forbidden by the typed API. }
  Transaction('clock-refuse-late', [TNyxDataValue.ParseJSON(
    '{"op":"title","value":"This title must not be accepted"}'),
    TNyxDataValue.ParseJSON('{"op":"value-domain-set","id":"meeting-start",' +
      '"domain":{"type":"text","format":"time","step":0}}')], True);
  Check(Snapshot = LAfter, 'a late invalid clock policy refuses the entire related group');
  History('undo');
  Check((Snapshot = LBefore) and (Domain.ToJSON = LDomainBefore.ToJSON),
    'one semantic Undo restores exact Pascal and paged clock policy');
  History('redo');
  Check((Snapshot = LAfter) and (Domain.Field('stepMilliseconds').AsInteger = 500),
    'one semantic Redo restores the admitted Pascal and clock policy');
  GAcceptedSource := Source;
  Save('nyx.generated.view.pas', GAcceptedSource);
  Save('design.nyx', GAcceptedPair.Design);
  Save('meeting-planner.nyxproject', EncodeNyxProject(GAcceptedPair));
  Save('clock-domain.json', Domain.ToJSON);
  LReply := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  LOutput := LReply.Field('outputID').AsText;
  for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
  begin
    for LScope := Low(TNyxBuildScope) to High(TNyxBuildScope) do
    begin
      Build(LTarget, LScope, LOutput);
    end;
  end;
  Observe;
  Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
    NyxField('review', NyxData(GReview)), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('clock-review-discard'))], False);
  GReview := '';
  FreeAndNil(GStudio);
  LReply := Call('nyx_session', [], False);
  for LIndex := 0 to LPrimary.Count - 1 do
  begin

    if LPrimary.Key(LIndex) <> 'activitySequence' then
    begin
      Check(LReply.Field(LPrimary.Key(LIndex)).ToJSON =
        LPrimary.Field(LPrimary.Key(LIndex)).ToJSON,
        'primary project remains unchanged: ' + LPrimary.Key(LIndex));
    end;
  end;
end;

begin
  GClient := nil;
  GStudio := nil;
  GRendered := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply configured MCP entry, fresh output directory and loopback editor HTTP base');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    GBase := TNyxText(ParamStr(3));
    { A caller may naturally supply the editor's URL with its trailing slash.
      Normalize it once before composing any owned preview/artifact request. }
    while (GBase <> '') and (GBase[Length(GBase)] = '/') do
    begin
      Delete(GBase, Length(GBase), 1);
    end;

    if DirectoryExists(GDirectory) then
    begin
      raise Exception.Create('The owned clock review output directory already exists');
    end;
    ForceDirectories(GDirectory);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty clock workshop');
    try
      Run;
    finally
      { Transport teardown retires this owner's remaining ephemeral review on
        failures too. It cannot discard another transport's project/review. }
      FreeAndNil(GRendered);
      FreeAndNil(GStudio);
      GClient.Close;
    end;
    WriteLn('PASS ', GChecks, ' authenticated clock authoring checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GClient.Free;
end.
