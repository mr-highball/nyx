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

program nyx_mcp_source_workflow;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Base64, nyx.text, nyx.bytes, nyx.data, nyx.studio.reviews,
  nyx.studio.builds, nyx.studio.buildjobs, nyx.studio.projects,
  nyx.studio.session, nyx.test.mcp.client, nyx.test.projection, nyx.state,
  nyx.binding.types, nyx.studio.stateedits, nyx.collections,
  nyx.studio.collectionedits, nyx.studio.resourceedits, nyx.resources,
  nyx.studio.transactions, nyx.types, nyx.editing;

const
  CInitialCaption: TNyxText = 'A place for your next idea';
  CAuthoredCaption: TNyxText = 'A thoughtfully crafted workspace';
  CImplementation: TNyxText = 'implementation' + #10;
  CThemeHelper: TNyxText = #10 +
    '{ Application theme helpers stay outside the managed view. }' + #10 +
    'type' + #10 +
    '  TWorkshopTheme = class' + #10 +
    '  public' + #10 +
    '    class function Caption: TNyxText;' + #10 +
    '  end;' + #10 + #10 +
    'class function TWorkshopTheme.Caption: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := ''A thoughtfully crafted workspace'';' + #10 +
    'end;' + #10 + #10 +
    'function WorkshopCaption: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := TWorkshopTheme.Caption;' + #10 +
    'end;' + #10;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxReviewRef;
  GRevision: Integer;
  GChecks: Integer;
  GEvidence: TNyxText;

procedure Save(const AName: TNyxText; const ABytes: RawByteString); forward;

{ Failed protocol operations report only their tool name. Credentials, user
  pairs and arbitrary server response bodies never enter console diagnostics. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithReview(AArguments, GReview));

  if LReply.Field('isError').AsBoolean then
  begin
    Save('refusal-' + ATool + '.json', LReply.ToJSON);
    raise Exception.Create('Source workflow refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source workflow: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Receipts are private qualification artifacts. The stream owns its handle;
  byte strings retain their exact UTF-8/PNG representation at this boundary. }
procedure Save(const AName: TNyxText; const ABytes: RawByteString);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[1], Length(ABytes));
    end;
  finally
    LStream.Free;
  end;
end;

{ Reconstruct only an explicitly owned review through revision-pinned scalar
  windows. An accepted copy may contain more than a starter page. This never
  pages the active user's primary unit or silently rebases between revisions. }
function Source: TNyxText;
var
  LWindow: TNyxDataValue;
  LOffset: Integer;
  LNext: Integer;
begin
  Check(GReview.ID <> '', 'source qualification requires its explicit owned review');
  Result := '';
  LOffset := 0;
  repeat
    LWindow := Call('nyx_pascal', NyxObject([
      NyxField('mode', NyxData('unit')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(2048))]));
    Result := Result + LWindow.Field('text').AsText;
    LNext := LWindow.Field('nextOffset').AsInteger;
    Check((LNext > LOffset) and (NyxTextScalarCount(Result) <= 262144),
      'owned source windows progress within their scalar budget');
    LOffset := LNext;
  until LOffset = LWindow.Field('total').AsInteger;
end;

function OffsetOf(const ASource, AAnchor: TNyxText): Integer;
var
  LPosition: Integer;
  LCursor: Integer;
  LScalar: Integer;
begin
  LPosition := Pos(AAnchor, ASource);
  Check(LPosition > 0, 'Exact source anchor exists');
  LCursor := 1;
  Result := 0;
  while LCursor < LPosition do
  begin

    if not NyxNextScalar(ASource, LCursor, LScalar) then
    begin
      raise Exception.Create('Malformed source Unicode');
    end;
    Inc(Result);
  end;
end;

{ Semantic jobs capture the review's exact pair. Poll bounded error diagnostics;
  timeouts fail without retrying a mutation or claiming successful execution. }
procedure Build(ATarget: TNyxBuildTarget; const AOutput: TNyxText;
  const ASeries: TNyxText = 'source-workshop'; const ASource: TNyxText = '');
var
  LTarget: TNyxText;
  LJob: TNyxText;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LTarget := NyxBuildTargetName(ATarget);
  LJob := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(ASeries + '-' + LTarget)),
    NyxField('outputID', NyxData(AOutput)),
    NyxField('target', NyxData(LTarget)),
    NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]));

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Source workshop compiler deadline exceeded');
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText));

  if ASeries = 'source-workshop' then
  begin
    Save('build-' + LTarget + '.json', LStatus.ToJSON);
  end
  else
  begin
    Save('build-' + ASeries + '-' + LTarget + '.json', LStatus.ToJSON);
  end;
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean, 'Exact authored ' + LTarget + ' job succeeds');

  if ASource <> '' then
  begin
    Check(LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(ASource),
      'Imported compiler job retains its exact accepted Pascal bytes');
  end;
  WriteLn('Compiled source workshop: ', LTarget);
  Flush(Output);
end;

{ Query only the affected published property. This proves paired design
  admission/restoration without exporting the complete document or source. }
procedure CheckCaption(const AExpected: TNyxText);
var
  LNode: TNyxDataValue;
begin
  LNode := Call('nyx_node', NyxObject([
    NyxField('id', NyxData('workshop-title')),
    NyxField('keys', NyxArray([NyxData('text')]))]));
  Check((LNode.Field('revision').AsInteger = GRevision) and
    (LNode.Field('properties').Count = 1) and
    (LNode.Field('properties').Item(0).Field('value').AsText = AExpected),
    'Published heading matches the source pair');
end;

{ Export only this owned review, never the primary project. Scalar windows
  remain pinned to one revision; joining them locally does not turn a semantic
  query into a whole-document response. The packet includes exact pending/base. }
function ReviewProjectPacket: TNyxText;
var
  LParts: TNyxStrings;
  LReply: TNyxDataValue;
  LOffset: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LReply := Call('nyx_project', NyxObject([
        NyxField('mode', NyxData('export')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(4096))]));
      LParts.Add(LReply.Field('text').AsText);
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

{ Opt-in focused data qualification stays in the existing transport-owned review
  after exact public compiler import. No current user project is used. Each tool
  carries its own exact retry identity and restores the complete imported pair. }
procedure FocusedReviewData(const AOutput: TNyxText);
var
  LDomain: TNyxTransactionDomain;
  LChanges: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LTool: TNyxText;
  LKey: TNyxCollectionRef;
  LStarted: QWord;
  LPair: TNyxProjectPair;
begin
  LKey := NyxCollection('review-notes');
  for LDomain := ntdState to ntdResources do
  begin
    LTool := NyxTransactionToolName(LDomain);
    case LDomain of
      ntdState:
        begin
          LChanges := NyxStateBindingPatch([
            NyxCreateDefault(NyxStateValue(NyxTextState('review-message'),
              'A workspace that listens.')),
            NyxBindControl(NyxBindingOwner('heading-1'), bpText,
              NyxTextState('review-message'), bdFromState)]).ToData;
        end;
      ntdCollections:
        begin
          LChanges := NyxCollectionPatch([
            NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('label'),
              'A useful note'), [NyxCollectionItem(NyxItem(LKey, 'first'))])]).ToData;
        end;
      ntdResources:
        begin
          LChanges := NyxResourcePatch([
            NyxDefineResource(NyxResourceRef('review-copy'), NyxDefaultLocale,
              NyxTextResource('Made with care.')),
            NyxBindResource(NyxControl('heading-1'), bpText,
              NyxResourceValue(NyxResourceRef('review-copy')))]).ToData;
        end;
    else
      begin
        raise Exception.Create('Focused HTTP qualification requires a data domain');
      end;
    end;
    LBefore := ReviewProjectPacket;
    LArguments := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('focused-' + LTool)), NyxField('changes', LChanges)]);
    LReceipt := Call(LTool, LArguments);
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'Focused data Apply supplies an unpublished constructor receipt');
    Check(Call(LTool, LArguments).ToJSON = LReceipt.ToJSON,
      'Focused HTTP retry retains one exact constructor job');
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', NyxObject([NyxField('mode', NyxData('status')),
        NyxField('job', LReceipt.Field('job')), NyxField('limit', NyxData(3))]));

      if LStatus.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 120000 then
      begin
        raise Exception.Create('Focused data constructor exceeded its deadline');
      end;
      Sleep(50);
    until False;
    Save('focused-' + LTool + '.json', LStatus.ToJSON);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger = GRevision + 1),
      'Focused HTTP constructor publishes the next exact revision');
    GRevision := LStatus.Field('publication').Field('revision').AsInteger;
    Check(Call(LTool, LArguments).ToJSON = LReceipt.ToJSON,
      'Focused completed HTTP retry keeps its original queued acknowledgement');
    LAfter := ReviewProjectPacket;
    LPair := DecodeNyxProject(LAfter);
    Check(Pos('class function TNotebookCards.Caption', LPair.Source) > 0,
      'Focused HTTP publication preserves the handwritten helper');

    if LDomain = ntdState then
    begin
      Check(Call('nyx_state', NyxObject([NyxField('mode', NyxData('value')),
        NyxField('name', NyxData('review-message')), NyxField('count', NyxData(64))]))
        .Field('text').AsText = 'A workspace that listens.',
        'Bounded state context exposes the exact authored default');
      Build(btBrowser, AOutput, 'focused-state', LPair.Source);
      Build(btNativeLCL, AOutput, 'focused-state', LPair.Source);
    end;
    Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('undo-' + LTool))]));
    GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    Check(ReviewProjectPacket = LBefore, 'One focused HTTP Undo restores the complete pair');
    Call('nyx_history', NyxObject([NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('redo-' + LTool))]));
    GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    Check(ReviewProjectPacket = LAfter, 'One focused HTTP Redo restores the constructed pair');
    Call('nyx_history', NyxObject([NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('restore-' + LTool))]));
    GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    Check(ReviewProjectPacket = LBefore, 'Focused HTTP review returns to its imported pair');
  end;
end;

{ Installed qualification uses the same already qualified reusable operations.
  All composition remains semantic and compiler admitted; no primary/project
  replacement, test-only source injection or new tool mode is involved. }
procedure ReusableReview;
var
  LBefore: TNyxText;
  LUndo: TNyxDataValue;
  LIndex: Integer;

  procedure Apply(const AID, AOperations: TNyxText);
  var
    LArguments: TNyxDataValue;
    LReceipt: TNyxDataValue;
    LStatus: TNyxDataValue;
    LStarted: QWord;
    LBeforeGroup: TNyxText;
    LAfterGroup: TNyxText;
  begin
    LBeforeGroup := ReviewProjectPacket;
    LArguments := NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
    LReceipt := Call('nyx_transaction', LArguments);
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'installed reusable group retains actual constructor admission');
    Check(Call('nyx_transaction', LArguments).ToJSON = LReceipt.ToJSON,
      'installed pending reusable retry retains its original receipt');
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
        NyxField('limit', NyxData(3))]));

      if LStatus.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 120000 then
      begin
        raise Exception.Create('Installed reusable constructor exceeded its budget');
      end;
      Sleep(50);
    until False;
    Save(AID + '-status.json', LStatus.ToJSON);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger = GRevision + 1),
      'installed reusable constructor publishes the exact next revision');
    GRevision := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
    Check(Call('nyx_transaction', LArguments).ToJSON = LReceipt.ToJSON,
      'installed completed reusable retry retains its original receipt');
    LAfterGroup := ReviewProjectPacket;
    Check((Pos('class function TNotebookCards.Caption', Source) > 0) and
      (Pos('.Padding(3 * 4)', Source) > 0), 'installed reuse retains computed Pascal/helpers');
    LStatus := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('undo')),
      NyxField('operationId', NyxData(AID + '-undo'))]));
    GRevision := LStatus.Field('revision').AsInteger;
    Check(ReviewProjectPacket = LBeforeGroup, 'one installed paired Undo restores the preceding pair');
    LStatus := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('redo')),
      NyxField('operationId', NyxData(AID + '-redo'))]));
    GRevision := LStatus.Field('revision').AsInteger;
    Check(ReviewProjectPacket = LAfterGroup, 'one installed paired Redo restores the exact reusable pair');
  end;

begin
  LBefore := ReviewProjectPacket;
  Apply('installed-derive',
    '[{"op":"derive","source":"reusable-note","id":"featured-note",' +
    '"identities":{"note-caption":"featured-caption"}},' +
    '{"op":"instance","component":"featured-note","id":"featured-1","parent":"notebook-1"},' +
    '{"op":"override","instance":"featured-1","id":"featured-title","path":"caption","mode":"properties"},' +
    '{"op":"update","id":"featured-title","properties":{"text":"Featured notebook"}}]');
  Apply('installed-inherit',
    '[{"op":"inherit","instance":"featured-1","id":"featured-title","path":"caption"}]');
  Apply('installed-extract',
    '[{"op":"derive","source":"heading-1","id":"page-heading","identities":{}},' +
    '{"op":"delete","id":"heading-1"},' +
    '{"op":"instance","component":"page-heading","id":"heading-1","parent":"notebook-1","index":0}]');
  Check(Pos('LHeading1Component: INyxComponent;', Source) > 0,
    'installed extraction emits the new specialized interface at the original identity');
  for LIndex := 1 to 3 do
  begin
    LUndo := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('undo')),
      NyxField('operationId', NyxData('installed-reuse-restore-' + IntToStr(LIndex)))]));
    GRevision := LUndo.Field('revision').AsInteger;
  end;
  Check(ReviewProjectPacket = LBefore, 'installed reusable qualification restores its exact imported review');
end;

{ Fork an admitted primary into a transport-owned review, then edit only that
  independent owner. Full parent/source/history preservation is additionally
  audited outside this consumer; no complete primary source is exported here. }
procedure AcceptedSeedReview(const APrimary: TNyxDataValue);
var
  LPreviousReview: TNyxReviewRef;
  LPreviousRevision: Integer;
  LReply: TNyxDataValue;
  LState: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LBeforePair: TNyxText;
  LAfterPair: TNyxText;
  LStarted: QWord;
begin
  LPreviousReview := GReview;
  LPreviousRevision := GRevision;
  try
    LReply := GClient.Tool('nyx_reviews', NyxObject([
      NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
      NyxField('expectedRevision', APrimary.Field('revision')),
      NyxField('operationId', NyxData('installed-accepted-seed')),
      NyxField('label', NyxData('Independent accepted source review'))]));
    Check(not LReply.Field('isError').AsBoolean, 'installed accepted source creates an independent review');
    GReview := NyxReview(LReply.Field('structuredContent').Field('review').AsText);
    LState := Call('nyx_session', NyxObject([]));
    GRevision := LState.Field('revision').AsInteger;
    Check((GRevision = 1) and not LState.Field('pendingDraft').AsBoolean and
      not LState.Field('canUndo').AsBoolean and not LState.Field('canRedo').AsBoolean,
      'installed accepted seed starts with fresh draft/history authority');
    LBefore := Source;
    LBeforePair := ReviewProjectPacket;
    LArguments := NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('installed-seed-title')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('title')), NyxField('value', NyxData('An independent review'))])]))]);
    LReceipt := Call('nyx_transaction', LArguments);
    Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
      'installed accepted checkpoint retains executed-source compiler authority');
    LStarted := GetTickCount64;
    repeat
      LState := Call('nyx_build', NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
        NyxField('limit', NyxData(3))]));

      if LState.Field('publication').Field('state').AsText <> 'pending' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 120000 then
      begin
        raise Exception.Create('Installed accepted seed exceeded its constructor budget');
      end;
      Sleep(50);
    until False;
    Save('installed-seed-status.json', LState.ToJSON);
    Check((LState.Field('state').AsText = 'succeeded') and
      (LState.Field('publication').Field('state').AsText = 'committed') and
      (LState.Field('publication').Field('revision').AsInteger = 2),
      'installed accepted seed publishes one real constructor result');
    GRevision := 2;
    LAfter := Source;
    LAfterPair := ReviewProjectPacket;
    LState := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('undo')),
      NyxField('operationId', NyxData('installed-seed-undo'))]));
    GRevision := LState.Field('revision').AsInteger;
    Check((Source = LBefore) and (ReviewProjectPacket = LBeforePair),
      'installed accepted seed owns its exact paired Undo');
    LState := Call('nyx_history', NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('direction', NyxData('redo')),
      NyxField('operationId', NyxData('installed-seed-redo'))]));
    GRevision := LState.Field('revision').AsInteger;
    Check((Source = LAfter) and (ReviewProjectPacket = LAfterPair),
      'installed accepted seed owns its exact paired Redo');
    LReply := GClient.Tool('nyx_reviews', NyxObject([
      NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview.ID)),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('installed-seed-discard'))]));
    Check(not LReply.Field('isError').AsBoolean and
      LReply.Field('structuredContent').Field('disposed').AsBoolean,
      'installed transport retires only its accepted seed');
  finally
    GReview := LPreviousReview;
    GRevision := LPreviousRevision;
  end;
end;

procedure ImportReviewProject(const AOutput: TNyxText);
var
  LAuthor: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LPreview: TNyxDataValue;
  LStatus: TNyxDataValue;
  LTools: TNyxDataValue;
  LSchema: TNyxDataValue;
  LView: TNyxText;
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LStarted: QWord;
  LIndex: Integer;
  LStart: Integer;
  LScalar: Integer;
  LCount: Integer;
  LOffset: Integer;
  LSerial: Integer;
begin
  Check(GReview.ID <> '', 'Project import requires an explicit owned review');
  LBefore := ReviewProjectPacket;
  LView := 'home';

  if ParamCount >= 3 then
  begin
    { The optional qualification fixture uses an independent whole-design oracle.
      Exact byte input reaches the installed public tool, never a trusted seed or
      ordinary user project. The fixture's first page is explicitly known. }
    LStream := TFileStream.Create(ParamStr(3), fmOpenRead or fmShareDenyNone);
    try

      if (LStream.Size < 1) or (LStream.Size > 1024 * 1024) then
      begin
        raise Exception.Create('Compiler import fixture exceeds its UTF-8 byte bound');
      end;
      SetLength(LBytes, LStream.Size);
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
      LPair := NyxProjectPair(ExpectedNyxProjectionDesign, NyxDecodeUTF8(LBytes));
    finally
      LStream.Free;
    end;
    LView := 'notebook-1';
    LTools := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
    Check(LTools.Count = 23, 'Installed authenticated discovery retains all 23 tools');
    LSchema := NyxNull;
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_project' then
      begin
        LSchema := LTools.Item(LIndex).Field('inputSchema');
      end;
    end;
    Save('installed-project-schema.json', LSchema.ToJSON);
    LArgs := LSchema.Field('oneOf').Item(4).Field('properties').Field('admission');
    Check((LArgs.Field('default').AsText = 'literal') and
      (LArgs.Field('enum').Count = 2) and
      (LArgs.Field('enum').Item(0).AsText = 'literal') and
      (LArgs.Field('enum').Item(1).AsText = 'compiler'),
      'Installed discovery advertises explicit compiler review with its literal default');
  end
  else
  begin
    LAuthor := TNyxStudioSession.Create;
    try
      LAuthor.AddPage;
      LAuthor.Document.Title := 'Project import review';
      LPair := LAuthor.ProjectSnapshot;
    finally
      LAuthor.Free;
    end;
  end;
  LPacket := EncodeNyxProject(LPair);
  Save('project-import-input.nyx', LPacket);
  LArgs := NyxObject([NyxField('mode', NyxData('begin-import')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('project-import-reserve')),
    NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]);
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed reserve retry is exact');
  LImport := LReply.Field('projectImport').Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  LSerial := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LCount := 0;
    while (LIndex <= Length(LPacket)) and (LCount < 4096) do
    begin

      if not NyxNextScalar(LPacket, LIndex, LScalar) then
      begin
        raise Exception.Create('Malformed project review input');
      end;
      Inc(LCount);
    end;
    Inc(LSerial);
    LArgs := NyxObject([NyxField('mode', NyxData('append-import')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('project-import-chunk-' + IntToStr(LSerial))),
      NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
      NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]);
    LReply := Call('nyx_project', LArgs);
    Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed chunk retry retains exact input');
    LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  Check(ReviewProjectPacket = LBefore, 'Installed staging preserves the accepted review pair');

  if ParamCount >= 3 then
  begin
    LArgs := NyxObject([NyxField('mode', NyxData('review-import')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('project-import-review')),
      NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match')),
      NyxField('admission', NyxData('compiler'))]);
  end
  else
  begin
    LArgs := NyxObject([NyxField('mode', NyxData('review-import')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('project-import-review')),
      NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match'))]);
  end;
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON,
    'Installed review retry retains its immediate ticket or one constructor job');

  if ParamCount >= 3 then
  begin
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReply.Field('job')),
        NyxField('limit', NyxData(3))]));

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 120000 then
      begin
        raise Exception.Create('Installed compiler project review exceeded its deadline');
      end;
      Sleep(50);
    until False;
    Save('project-import-construction.json', LStatus.ToJSON);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'reviewed') and
      (LStatus.Field('publication').Field('revision').AsInteger = GRevision) and
      not LStatus.Field('currentSource').AsBoolean,
      'Installed constructor supplies a ready review without applied source authority');
    Check(ReviewProjectPacket = LBefore, 'Installed compiler review leaves the current pair exact');
    LReply := NyxObject([NyxField('projectImport', LStatus.Field('projectImport'))]);
  end;
  LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
  Check(LReply.Field('projectImport').Field('candidate').Field('pages').AsInteger = 2,
    'Installed bounded review describes the two-page input');
  LArgs := NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('project-import-apply')),
    NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]);
  LReply := Call('nyx_project', LArgs);
  GRevision := LReply.Field('revision').AsInteger;
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed apply retry adds no history');
  Check(ReviewProjectPacket = LPacket, 'Installed admission retains the exact authored project pair');
  Build(btBrowser, AOutput, 'project-import', LPair.Source);
  Build(btNativeLCL, AOutput, 'project-import', LPair.Source);
  LPreview := GClient.Tool('nyx_preview', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('view', NyxData(LView)),
    NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
    NyxField('capture', NyxData(True))]), GReview));

  if LPreview.Field('isError').AsBoolean or (LPreview.Field('content').Count <> 3) then
  begin
    Save('project-import-preview-refusal.json', LPreview.ToJSON);
  end;
  Check(not LPreview.Field('isError').AsBoolean and (LPreview.Field('content').Count = 3),
    'Selective imported review preview returns its rendered PNG');
  Save('project-import-preview.png',
    DecodeStringBase64(LPreview.Field('content').Item(2).Field('data').AsText));

  if ParamCount = 4 then
  begin
    FocusedReviewData(AOutput);
    ReusableReview;
  end;
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('undo-project-import')),
    NyxField('direction', NyxData('undo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LBefore, 'One installed Undo restores the exact previous pair');
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('redo-project-import')),
    NyxField('direction', NyxData('redo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LPacket, 'One installed Redo restores the exact imported pair');
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('restore-review-before-import')),
    NyxField('direction', NyxData('undo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LBefore, 'Owned review returns to its original workshop');
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxText;
  LCaption: TNyxText;
  LPreview: TNyxDataValue;
  LOutput: TNyxText;
  LArguments: TNyxDataValue;
begin
  { Reviews belong to this authenticated transport. Teardown retires them even
    on failure; neither creation nor grouped source Undo touches the primary. }
  LPrimary := Call('nyx_session', NyxObject([]));
  LReceipt := Call('nyx_reviews', NyxObject([
    NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData('source-workshop')),
    NyxField('base', NyxData('empty')), NyxField('label', NyxData('Source workshop'))]));
  GReview := NyxReview(LReceipt.Field('review').AsText);
  GRevision := LReceipt.Field('session').Field('revision').AsInteger;
  LReceipt := Call('nyx_transaction', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('compose-source-workshop')),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('page')),
        NyxField('id', NyxData('workshop')), NyxField('root', NyxData('page'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('heading')),
        NyxField('id', NyxData('workshop-title')), NyxField('parent', NyxData('workshop')),
        NyxField('properties', NyxObject([NyxField('text', NyxData(CInitialCaption))]))])]))]));
  GRevision := LReceipt.Field('revision').AsInteger;
  LBefore := Source;
  LCaption := '''' + CInitialCaption + '''';
  LArguments := NyxObject([
    NyxField('mode', NyxData('edit-unit')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('author-theme-and-view')),
    NyxField('changes', NyxArray([
      NyxObject([NyxField('offset', NyxData(OffsetOf(LBefore, CImplementation))),
        NyxField('expected', NyxData(CImplementation)),
        NyxField('replacement', NyxData(CImplementation + CThemeHelper))]),
      NyxObject([NyxField('offset', NyxData(OffsetOf(LBefore, LCaption))),
        NyxField('expected', NyxData(LCaption)),
        NyxField('replacement', NyxData('''' + CAuthoredCaption + ''''))])]))]);
  LReceipt := Call('nyx_pascal', LArguments);
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(LReceipt.Field('sourceEdit').Field('changes').AsInteger = 2,
    'Class/helper/view change is one semantic group');
  Check(Call('nyx_pascal', LArguments).ToJSON = LReceipt.ToJSON,
    'Exact authenticated retry creates no duplicate history step');
  Check(Pos(CThemeHelper, Source) > 0, 'Handwritten class/helper survives admission');
  CheckCaption(CAuthoredCaption);
  LOutput := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  Build(btBrowser, LOutput);
  Build(btNativeLCL, LOutput);
  LPreview := GClient.Tool('nyx_preview', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('view', NyxData('workshop')),
    NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
    NyxField('capture', NyxData(True))]), GReview));

  if LPreview.Field('isError').AsBoolean or (LPreview.Field('content').Count <> 3) then
  begin
    { Retain the real refusal before closing this transport's private review.
      Never replace a failed capture with a URL or successful compilation claim. }
    Save('semantic-preview-refusal.json', LPreview.ToJSON);
  end;
  Check(not LPreview.Field('isError').AsBoolean and
    (LPreview.Field('content').Count = 3), 'Selective semantic preview returns its PNG');
  Save('semantic-preview.png',
    DecodeStringBase64(LPreview.Field('content').Item(2).Field('data').AsText));
  LReceipt := Call('nyx_history', NyxObject([
    NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('undo-theme-and-view'))]));
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(Source = LBefore, 'One paired Undo restores the exact source');
  CheckCaption(CInitialCaption);
  ImportReviewProject(LOutput);
  CheckCaption(CInitialCaption);

  if ParamCount = 4 then
  begin
    AcceptedSeedReview(LPrimary);
  end;
  GReview := NyxActiveWorkspace;
  Check(Call('nyx_session', NyxObject([])).Field('revision').AsInteger =
    LPrimary.Field('revision').AsInteger, 'Primary revision remains unchanged');
end;

begin
  GClient := nil;
  GReview := NyxActiveWorkspace;
  try
    try

      if not (ParamCount in [2, 3, 4]) or
        ((ParamCount = 4) and (ParamStr(4) <> '--focused-data')) then
      begin
        raise Exception.Create('Supply existing local MCP config, NEW private evidence directory ' +
          'and optional exact handwritten qualification unit with --focused-data');
      end;
      GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
      Check(not DirectoryExists(GEvidence), 'Evidence directory is new');
      ForceDirectories(GEvidence);
      GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty source workshop');
      Run;
      WriteLn('PASS ', GChecks, ' authenticated source workflow checks');
    except
      on LException: Exception do
      begin
        WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
        ExitCode := 1;
      end;
    end;
  finally
    try

      if GClient <> nil then
      begin
        { A plain TObject.Free does not retire a remote MCP transport/review.
          Explicit DELETE releases private authority; Close is idempotent. }
        GClient.Close;
      end;
    finally
      GClient.Free;
    end;
  end;
end.
