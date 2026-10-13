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

program nyx_tabs_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.state,
  nyx.binding.types, nyx.studio.stateedits,
  nyx.studio.projects, nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ One owning authenticated transport keeps its empty review through grouped
  composition, bounded export, exact paired history and explicit retirement.
  Credentials never enter source, output project bytes or application URLs. }
function Call(const ATool: TNyxText; const AFields: array of TNyxDataField;
  AOwned: Boolean = True): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LReply: TNyxDataValue;
begin
  SetLength(LFields, Length(AFields) + Ord(AOwned));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := AFields[LIndex];
  end;

  if AOwned then
  begin
    LFields[High(LFields)] := NyxField('review', NyxData(GReview));
  end;
  LReply := GClient.Tool(ATool, NyxObject(LFields));
  Check(not LReply.Field('isError').AsBoolean,
    ATool + ' refused: ' + Copy(LReply.ToJSON, 1, 700));
  Result := LReply.Field('structuredContent');
end;

function ExportPair: TNyxProjectPair;
var
  LReply: TNyxDataValue;
  LText: TNyxText;
  LOffset: Integer;
begin
  LText := '';
  LOffset := 0;
  repeat
    LReply := Call('nyx_project', [NyxField('mode', NyxData('export')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('part', NyxData('project')),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]);
    Check(LReply.Field('revision').AsInteger = GRevision, 'bounded export keeps its revision');
    LText := LText + LReply.Field('text').AsText;
    LOffset := LReply.Field('nextOffset').AsInteger;
  until LOffset = LReply.Field('total').AsInteger;
  Result := DecodeNyxProject(LText);
end;

procedure Save(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

procedure History(const ADirection: TNyxText);
begin
  Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('tabs-' + ADirection + '-' + TNyxText(IntToStr(GRevision))))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure CaptureInstalledBuild;
var
  LOutputs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
  LState: TNyxText;
begin
  { The installed compiler owns its ordinary profile/source snapshot. Capture
    readiness and diagnostics rather than overriding an older deployment to
    claim the current adapters. A successful build alone does not run tabs. }
  LOutputs := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  Save('installed-readiness.json', LOutputs.ToJSON);
  LReceipt := GClient.Tool('nyx_build', NyxObject([
    NyxField('review', NyxData(GReview)), NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('tabs-installed-build')),
    NyxField('outputID', LOutputs.Field('outputID')),
    NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application'))]));
  Save('installed-build-request.json', LReceipt.ToJSON);

  if LReceipt.Field('isError').AsBoolean then
  begin
    Exit;
  end;
  LReceipt := LReceipt.Field('structuredContent');
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', LReceipt.Field('job')), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(6))]);
    Save('installed-build-status.json', LStatus.ToJSON);
    LState := LStatus.Field('state').AsText;

    if (LState <> 'queued') and (LState <> 'running') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Installed compiler is unfinished; retain its receipt for observation');
    end;
    Sleep(200);
  until False;
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LAfter: TNyxDataValue;
  LEmpty: TNyxProjectPair;
  LPair: TNyxProjectPair;
  LChanges: TNyxDataValue;
  LUnbound: TNyxProjectPair;
  LPatch: INyxStateBindingPatch;
  LBindingReply: TNyxDataValue;
  LEdited: TNyxProjectPair;
  LEditedDocument: TNyxDocument;
begin
  Check((ParamCount = 2) or ((ParamCount = 3) and
    ((ParamStr(3) = '--selection-binding') or (ParamStr(3) = '--pages-authoring'))),
    'Supply explicit config.toml, a NEW export directory and optional tab qualification mode');
  Check(not DirectoryExists(ParamStr(2)), 'Companion output must be new');
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty retained tabs companion');
  try
    LPrimary := Call('nyx_session', [], False);
    GReview := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('A place for every idea')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData('tabs-review-create'))], False).Field('review').AsText;
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LEmpty := ExportPair;
    { Static protocol intent is admitted once at the explicit MCP boundary.
      The installed release generates specialized interfaces and typed fluent
      properties. New tab policy authoring is qualified separately in source. }
    LChanges := TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"page","id":"ideas","root":"page","properties":{"text":"Ideas","padding":24,"gap":16}},' +
      '{"op":"create","kind":"heading","id":"ideas-heading","parent":"ideas","properties":{"text":"A place for every idea"}},' +
      '{"op":"create","kind":"label","id":"ideas-intro","parent":"ideas","properties":{"text":"Switch sections without losing the notes you are writing."}},' +
      '{"op":"create","kind":"tabs","id":"idea-sections","parent":"ideas","properties":{"text":"Idea sections","height":440}},' +
      '{"op":"create","kind":"tab","id":"plan","parent":"idea-sections","properties":{"text":"Plan","padding":20,"gap":12}},' +
      '{"op":"create","kind":"heading","id":"plan-heading","parent":"plan","properties":{"text":"Start with a small sketch"}},' +
      '{"op":"create","kind":"memo","id":"plan-notes","parent":"plan","properties":{"text":"Planning notes","placeholder":"What would you like to make?","height":200}},' +
      '{"op":"create","kind":"tab","id":"details","parent":"idea-sections","properties":{"text":"Details","padding":20,"gap":12}},' +
      '{"op":"create","kind":"heading","id":"details-heading","parent":"details","properties":{"text":"Make the details yours"}},' +
      '{"op":"create","kind":"input","id":"idea-title","parent":"details","properties":{"text":"Project title","placeholder":"A brilliant little project"}},' +
      '{"op":"create","kind":"tab","id":"review","parent":"idea-sections","properties":{"text":"Review","padding":20,"gap":12}},' +
      '{"op":"create","kind":"heading","id":"review-heading","parent":"review","properties":{"text":"Keep making things"}},' +
      '{"op":"create","kind":"label","id":"review-copy","parent":"review","properties":{"text":"Your notes and caret stay with their page. Return to Plan whenever you need."}}]');
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('tabs-compose')), NyxField('operations', LChanges)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LPair := ExportPair;
    Check((Pos('INyxTabs', LPair.Source) > 0) and (Pos('INyxTab', LPair.Source) > 0) and
      (Pos('NewNyxMemo', LPair.Source) > 0), 'companion uses specialized reference-counted controls');
    History('undo');
    Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LEmpty), 'one Undo restores the exact prior pair');
    History('redo');
    Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair), 'one Redo restores the exact composed pair');
    ForceDirectories(ParamStr(2));

    if (ParamCount = 3) and (ParamStr(3) = '--pages-authoring') then
    begin
      Save('installed-pages-context.json', Call('nyx_node', [
        NyxField('id', NyxData('idea-sections')), NyxField('limit', NyxData(3)),
        NyxField('events', NyxData(False))]).ToJSON);
      { Existing compact semantic operations manage pages without returning an
        internal form baseline. Related changes share one revision/Undo step. }
      LChanges := TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"tab","id":"extra-notes","parent":"idea-sections","properties":{"text":"Extra notes"}},' +
        '{"op":"update","id":"plan","properties":{"text":"Planning"}},' +
        '{"op":"move","id":"review","parent":"idea-sections","index":0},' +
        '{"op":"delete","id":"details"}]');
      Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('tabs-page-management')), NyxField('operations', LChanges)]);
      GRevision := Call('nyx_session', []).Field('revision').AsInteger;
      LEdited := ExportPair;
      LEditedDocument := TNyxCodec.Decode(LEdited.Design);
      try
        Check((LEditedDocument.Find('idea-sections').Count = 3) and
          (LEditedDocument.Find('idea-sections').Children[0].ID = 'review') and
          (LEditedDocument.Find('plan').Prop('text') = 'Planning') and
          (LEditedDocument.Find('extra-notes') <> nil) and
          (LEditedDocument.Find('idea-title') = nil),
          'semantic page group creates, renames, reorders and removes the exact subtree');
      finally
        LEditedDocument.Free;
      end;
      Save('page-edits.nyx', EncodeNyxProject(LEdited));
      History('undo');
      Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair),
        'one semantic Undo restores the complete pre-management pair');
      History('redo');
      Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LEdited),
        'one semantic Redo restores all related page edits exactly');
      History('undo');
      Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair),
        'ordinary control qualification consumes the exact original semantic companion');
    end;

    if (ParamCount = 3) and (ParamStr(3) = '--selection-binding') then
    begin
      { Use the installed semantic operation first. An older adapter/schema may
        refuse Value on tabs; keep the exact refusal and unbound companion rather
        than replacing the deployment or claiming installed integration. }
      LUnbound := LPair;
      LPatch := NyxStateBindingPatch([
        NyxCreateDefault(NyxStateValue(NyxTextState('selected-section'), 'plan')),
        NyxBindControl(NyxBindingOwner('idea-sections'), bpValue,
          NyxTextState('selected-section'), bdTwoWay)]);
      Save('installed-binding-context.json', Call('nyx_state', [
        NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('idea-sections')),
        NyxField('offset', NyxData(0)), NyxField('limit', NyxData(20))]).ToJSON);
      LBindingReply := GClient.Tool('nyx_state', NyxObject([
        NyxField('review', NyxData(GReview)), NyxField('mode', NyxData('apply')),
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('tabs-selection-binding')),
        NyxField('changes', LPatch.ToData)]));
      Save('installed-binding-result.json', LBindingReply.ToJSON);

      if LBindingReply.Field('isError').AsBoolean then
      begin
        Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LUnbound),
          'installed binding refusal preserves the exact unbound pair');
      end
      else
      begin
        GRevision := Call('nyx_session', []).Field('revision').AsInteger;
        LPair := ExportPair;
        Check(Pos('selected-section', LPair.Source) > 0,
          'installed semantic export contains the selection default and binding');
        History('undo');
        Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LUnbound),
          'one Undo removes the complete state/binding group');
        History('redo');
        Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair),
          'one Redo restores the exact bound companion');
      end;
      LPatch := nil;
    end;
    Save('nyx.generated.view.pas', LPair.Source);
    Save('tabs.nyx', EncodeNyxProject(LPair));
    CaptureInstalledBuild;
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview)),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('tabs-review-discard'))], False);
    GReview := '';
    LAfter := Call('nyx_session', [], False);
    Check((LPrimary.Field('revision').ToJSON = LAfter.Field('revision').ToJSON) and
      (LPrimary.Field('selection').ToJSON = LAfter.Field('selection').ToJSON) and
      (LPrimary.Field('pendingDraft').ToJSON = LAfter.Field('pendingDraft').ToJSON) and
      (LPrimary.Field('canUndo').ToJSON = LAfter.Field('canUndo').ToJSON) and
      (LPrimary.Field('canRedo').ToJSON = LAfter.Field('canRedo').ToJSON),
      'primary revision, selection, draft and history remain exact');
  finally
    GClient.Close;
    GClient.Free;
    GClient := nil;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' semantic tab composition/export/paired-history checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
