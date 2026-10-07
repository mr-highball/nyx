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

program nyx_studio_legacy_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}Web,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.workspaces, nyx.studio.legacy;

var
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
  LDraft: TNyxProjectPair;
  LSnapshot: TNyxLegacyStudioSnapshot;
  LReport: TNyxDataValue;
  LFrame: TNyxAgentRecoveryFrame;
  LRefused: Boolean;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

function Entry(const AWorkspace, AView, APermission: TNyxText;
  const APair: TNyxProjectPair; AHistory: Boolean): TNyxDataValue;
begin
  Result := NyxObject([NyxField('workspace', NyxData(AWorkspace)),
    NyxField('label', NyxData('Existing workshop')),
    NyxField('project', NyxData(EncodeNyxProject(APair))),
    NyxField('session', NyxObject([NyxField('revision', NyxData(5)),
      NyxField('permission', NyxData(APermission)), NyxField('selection', NyxData('notes')),
      NyxField('view', NyxData(AView)), NyxField('pendingDraft', NyxData(APair.Pending)),
      NyxField('canUndo', NyxData(AHistory)), NyxField('canRedo', NyxData(False))]))]);
end;

procedure Refuse(const AEntries: TNyxDataValue; APolicy: TNyxLegacyHistoryPolicy;
  const AIdentity, AReason: TNyxText);
var
  LCandidate: TNyxLegacyStudioSnapshot;
  LRejected: Boolean;
begin
  LCandidate := nil;
  LRejected := False;
  try
    try
      LCandidate := TNyxLegacyStudioSnapshot.Create(AEntries, APolicy, AIdentity);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, AReason);
  finally
    LCandidate.Free;
  end;
end;

begin
  LDocument := nil;
  LSnapshot := nil;
  try
    LDocument := TNyxDocument.Create;
    LDocument.AddPage(NewNyxPage('home').Configure.Text('Existing page').Done);
    LDocument.Pages[0].Add(NewNyxMemo('notes').Configure.Value('Exact notes 😀').Done);
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LDraft := LSeed;
    LDraft.Pending := True;
    LDraft.Draft := '';
    LDraft.DraftBase := LSeed.Source;
    Refuse(NyxArray([Entry('', 'home', 'edit', LSeed, True)]),
      nlhRequireEmptyHistory, 'new', 'Reported history cannot disappear under default admission');
    Refuse(NyxArray([Entry('', 'missing', 'edit', LSeed, False)]),
      nlhResetTestHistory, 'new', 'Foreign navigation refuses before publishing a registry');
    Refuse(NyxArray([Entry('', 'home', 'unknown', LSeed, False)]),
      nlhResetTestHistory, 'new', 'Unknown permission refuses rather than enabling agents');
    Refuse(NyxArray([Entry('', 'home', 'edit', LSeed, False),
      Entry('old.project-1', 'home', 'edit', LSeed, False)]),
      nlhResetTestHistory, 'old', 'Reusing the legacy creation epoch refuses');
    Refuse(NyxArray([Entry('', 'home', 'edit', LSeed, False),
      Entry('old.project-1', 'home', 'edit', LSeed, False),
      Entry('old.project-1', 'home', 'edit', LSeed, False)]),
      nlhResetTestHistory, 'new', 'Duplicate old handles refuse as ambiguous');
    LSnapshot := TNyxLegacyStudioSnapshot.Create(
      NyxArray([Entry('', 'home', 'readOnly', LDraft, False),
        Entry('old.project-1', 'home', 'edit', LSeed, True)]),
      nlhResetTestHistory, 'new');
    LReport := LSnapshot.Report;
    Check((LReport.Field('historyResetCount').AsInteger = 1) and
      LReport.Field('namingCountersReset').AsBoolean,
      'Every unavailable reset is disclosed without claiming complete migration');
    LFrame := LSnapshot.Primary.RecoveryFrame;
    Check((EncodeNyxProject(LFrame.Session.Pair) = EncodeNyxProject(LDraft)) and
      LFrame.Session.Pair.Pending and (LFrame.Session.Pair.Draft = ''),
      'Exact accepted Unicode source and defined empty pending draft survive');
    Check((LFrame.Revision = 5) and (LFrame.Permission = apReadOnly) and
      (LFrame.Session.Selection = 'notes') and (LFrame.Session.View = 'home'),
      'Public revision, permission and exact navigation survive');
    Check((Length(LFrame.Session.Undo) = 0) and (Length(LFrame.Session.Redo) = 0),
      'The primary empty history remains empty');
    Check((LReport.Field('mapping').Item(1).Field('previousWorkspace').AsText =
      'old.project-1') and (LReport.Field('mapping').Item(1).Field('workspace').AsText =
      'new.project-1'), 'Ordinary handles receive an explicit non-aliasing mapping');
    LRefused := False;
    try
      LSnapshot.Workspaces.Resolve(NyxWorkspace('old.project-1'));
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Retired ordinary handles cannot fall back to primary');
    Check(LSnapshot.Workspaces.Resolve(NyxWorkspace('new.project-1')).Permission =
      apReadOnly, 'The restored operator permission still governs ordinary projects');
    { Explicit owned-controller enablement; the importer itself never enables a
      restored read-only operator. Pair/draft and public revision stay untouched. }
    LSnapshot.Primary.InheritPermission(apEdit);
    LSnapshot.Workspaces.Resolve(NyxWorkspace('new.project-1')).Call('nyx_transaction',
      'Legacy fixture', NyxObject([NyxField('expectedRevision', NyxData(5)),
        NyxField('operationId', NyxData('new-history')),
        NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('title')),
          NyxField('value', NyxData('New workshop title'))])]))]));
    Check(LSnapshot.Workspaces.Resolve(NyxWorkspace('new.project-1')).RecoveryFrame
      .Session.Pair.Design <> LSeed.Design, 'The admitted project builds fresh paired history');
    LSnapshot.Workspaces.Resolve(NyxWorkspace('new.project-1')).Call('nyx_history',
      'Legacy fixture', NyxObject([NyxField('expectedRevision', NyxData(6)),
        NyxField('operationId', NyxData('new-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LSnapshot.Workspaces.Resolve(NyxWorkspace('new.project-1'))
      .RecoveryFrame.Session.Pair) = EncodeNyxProject(LSeed),
      'One fresh semantic Undo restores the exact preserved workshop pair');
    Check(EncodeNyxProject(LSnapshot.Primary.RecoveryFrame.Session.Pair) =
      EncodeNyxProject(LDraft), 'Independent project edits never replace the primary draft');
    FreeAndNil(LSnapshot);
    LSnapshot := TNyxLegacyStudioSnapshot.Create(
      NyxArray([Entry('', 'home', 'edit', LSeed, False)]),
      nlhRequireEmptyHistory, 'empty-new');
    Check(LSnapshot.HistoryResetCount = 0, 'A genuinely empty legacy history needs no reset');
    WriteLn('PASS ', LChecks, ' legacy snapshot admission checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-legacy-snapshot', 'passed');
    document.body.setAttribute('data-legacy-snapshot-checks', IntToStr(LChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-legacy-snapshot', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LSnapshot.Free;
  LDocument.Free;
end.
