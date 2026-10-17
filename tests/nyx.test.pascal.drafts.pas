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
unit nyx.test.pascal.drafts;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Pure portable draft/reference checks. No source is executed, no file is read
  and no UI/transport is substituted for an execution or interaction claim. }
function RunNyxPascalDraftChecks: Integer;
{ Inventory/form/strict private-intent contracts only. Actual construction and
  ordinary controller input remain separate maintained consumers. }
function RunNyxPascalFileChangeChecks: Integer;
{ Exact multi-file range/Unicode/value/budget admission, without executing code.
  Real compiler, semantic publication and observing input are separate checks. }
function RunNyxPascalProjectEditChecks: Integer;
{ Include-aware lexical ownership/physical locations and typed body preparation;
  these are data checks, not application execution or ordinary editor evidence. }
function RunNyxPascalRoutineIndexChecks: Integer;

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.editing, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.filechanges,
  nyx.pascal.files.editor, nyx.controls, nyx.studio.sourcebuilds,
  nyx.studio.sourceedits, nyx.source, nyx.studio.routineedits, nyx.studio.session;

function RunNyxPascalDraftChecks: Integer;
const
  CUnit: TNyxText = 'unit draft.values;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface' + #10 +
    'implementation' + #10 + 'end.' + #10;
  CMain: TNyxText = 'unit draft.views;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface' + #10 +
    'implementation' + #10 + 'end.' + #10;
  CInclude: TNyxText = '{ An accepted include 🚀 }' + #10;
  CDraft: TNyxText = '{ An unfinished helper 𐐷' + #0 + #10;
var
  LProject: INyxPascalProject;
  LUnit: TNyxPascalSourceRef;
  LInclude: TNyxPascalSourceRef;
  LEmpty: TNyxPascalFileDrafts;
  LFirst: TNyxPascalFileDrafts;
  LBoth: TNyxPascalFileDrafts;
  LChanged: TNyxPascalFileDrafts;
  LDecoded: TNyxPascalFileDrafts;
  LData: TNyxDataValue;
  LRejected: Boolean;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LProject := NewNyxPascalProject(NyxPascalUnit('draft.views'), CMain)
    .WithUnit(NyxPascalUnit('draft.values'), CUnit)
    .WithInclude(NyxPascalInclude('draft-body.inc'), CInclude);
  LProject.Validate;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('draft.values'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('draft-body.inc'));
  Check(TNyxPascalSourceRef.Main.Text(LProject) = CMain, 'main reference resolves exact main');
  Check((LUnit.Name = 'draft.values.pas') and (LUnit.Text(LProject) = CUnit),
    'typed unit reference keeps exact role/name/text');
  Check((LInclude.Name = 'draft-body.inc') and (LInclude.Text(LProject) = CInclude),
    'typed include reference keeps exact role/name/text');
  LEmpty := Default(TNyxPascalFileDrafts);
  Check((LEmpty.Count = 0) and (LEmpty.Encoded = '') and (LEmpty.StorageBytes = 0),
    'default helper buffers retain absent legacy state');
  LFirst := LEmpty.Editing(LUnit, CUnit, CDraft);
  Check((LEmpty.Count = 0) and (LFirst.Count = 1), 'first edit leaves its original value unchanged');
  Check((LFirst.Draft(LUnit).Text = CDraft) and (LFirst.Draft(LUnit).Base = CUnit),
    'unfinished Unicode/NUL text retains its exact accepted base');
  LBoth := LFirst.Editing(LInclude, CInclude, '');
  Check((LBoth.Count = 2) and (LFirst.Count = 1), 'interleaved include keeps independent unit data');
  Check(LBoth.Contains(LInclude) and (LBoth.Draft(LInclude).Text = ''),
    'an empty helper buffer remains distinguishable from no buffer');
  LChanged := LBoth.Editing(LUnit, '{ A newer accepted version }', '{ A newer draft }');
  Check((LChanged.Draft(LUnit).Base = CUnit) and (LBoth.Draft(LUnit).Text = CDraft),
    'editing a copied record neither rebases nor mutates an older value');
  Check(LChanged.Draft(LInclude).Base = CInclude, 'another helper base stays independent');
  LChanged.ValidateAgainst(LProject);
  Check(True, 'unfinished text needs membership, not Pascal interpretation');
  LDecoded := TNyxPascalFileDrafts.Parse(LBoth.Encoded);
  Check(LDecoded.Encoded = LBoth.Encoded, 'complete encoded buffers round-trip exactly');
  Check((LDecoded.Draft(LUnit).Text = CDraft) and (LDecoded.Draft(LInclude).Text = ''),
    'Unicode/NUL and empty fields survive the boundary');
  Check(LDecoded.StorageBytes = NyxUTF8ByteCount(LUnit.Name) +
    NyxUTF8ByteCount(CDraft) + NyxUTF8ByteCount(CUnit) +
    NyxUTF8ByteCount(LInclude.Name) + NyxUTF8ByteCount(CInclude),
    'buffer storage accounts for exact reference/text/base bytes');
  LChanged := LBoth.Without(LUnit);
  Check((LChanged.Count = 1) and LChanged.Contains(LInclude) and (LBoth.Count = 2),
    'discard removes only the acknowledged helper in a new value');
  Check(LChanged.Without(LUnit).Encoded = LChanged.Encoded, 'missing discard is idempotent');
  LChanged := LBoth.Editing(LUnit, CUnit, CUnit);
  Check((LChanged.Count = 1) and LChanged.Contains(LInclude),
    'returning to current acceptance closes only that buffer');
  LRejected := False;
  try
    LEmpty.Editing(TNyxPascalSourceRef.Main, CMain, CDraft);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'main edits retain their established independent contract');
  LRejected := False;
  try
    TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('absent.inc')).Text(LProject);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'unknown typed member refuses without touching buffers');
  LData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('items', NyxArray([
      NyxObject([NyxField('kind', NyxData('main')), NyxField('name', NyxData('')),
        NyxField('text', NyxData(CDraft)), NyxField('base', NyxData(CMain))])]))]);
  LRejected := False;
  try
    TNyxPascalFileDrafts.Parse(LData.ToJSON);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'wire input cannot hide a second main buffer among helpers');
  LData := TNyxDataValue.ParseJSON(LBoth.Encoded);
  LData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('items', NyxArray([LData.Field('items').Item(0),
      LData.Field('items').Item(0)]))]);
  LRejected := False;
  try
    TNyxPascalFileDrafts.Parse(LData.ToJSON);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'duplicate helper identity refuses atomically');
  Check(LBoth.Draft(LUnit).Text = CDraft, 'all refusal cases retain prior values');
  Result := LChecks;
end;

function RunNyxPascalFileChangeChecks: Integer;
const
  CMain: TNyxText = 'unit files.views;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface' + #10 +
    'implementation' + #10 + 'end.' + #10;
  CInclude: TNyxText = '{ Exact helper 🚀 𐐷 }' + #10;
var
  LBase: INyxPascalProject;
  LUnit: INyxPascalProject;
  LBoth: INyxPascalProject;
  LRemoved: INyxPascalProject;
  LChange: TNyxPascalFileChange;
  LCopy: TNyxPascalFileChange;
  LWire: TNyxDataValue;
  LRead: TNyxPascalFileChange;
  LForm: INyxCard;
  LState: TNyxPascalFilesEditorState;
  LRejected: Boolean;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LBase := NewNyxPascalProject(NyxPascalUnit('files.views'), CMain);
  LChange := TNyxPascalFileChange.CreateUnit(NyxPascalUnit('files.helpers'));
  LUnit := LChange.Candidate(LBase);
  Check((LBase.Count = 0) and (LUnit.Count = 1) and (LUnit.Source = CMain),
    'creation owns a new inventory without editing old values or uses clauses');
  Check((LUnit.Input(0).Name = 'files.helpers.pas') and
    (Pos('{$mode delphi}{$H+}{$codepage utf8}', LUnit.Input(0).Text) > 0),
    'a typed unit name supplies the exact Delphi/UTF-8 template');
  LCopy := TNyxPascalFileChange.CreateInclude(NyxPascalInclude('notes.inc')).WithText(CInclude);
  LBoth := LCopy.Candidate(LUnit);
  Check((LBoth.Count = 2) and (LUnit.Count = 1) and (LBoth.Input(1).Text = CInclude),
    'independent include creation retains supplementary text and authored order');
  LRemoved := TNyxPascalFileChange.Remove(LChange.FileReference).Candidate(LBoth);
  Check((LRemoved.Count = 1) and (LRemoved.Input(0).Name = 'notes.inc') and
    (LBoth.Count = 2), 'removal preserves the other helper and original snapshot');
  LRemoved := TNyxPascalFileChange.Remove(LCopy.FileReference).Candidate(LRemoved);
  Check((LRemoved.Count = 0) and not TNyxPascalProjectFiles.Capture(LRemoved).IsEmpty and
    (TNyxPascalProjectFiles.Capture(LRemoved).Open(CMain).Count = 0),
    'removing the last helper retains the explicit project and its empty inventory');
  LRejected := False;
  try
    LChange.Candidate(LUnit);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LUnit.Count = 1), 'Create never replaces an existing helper');
  LRejected := False;
  try
    TNyxPascalFileChange.CreateUnit(NyxPascalUnit('FILES.HELPERS')).Candidate(LUnit);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'case aliases cannot overwrite a captured identity');
  LRejected := False;
  try
    TNyxPascalFileChange.Remove(LChange.FileReference).Candidate(LBase);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'missing removal refuses instead of inventing a history command');
  LRejected := False;
  try
    TNyxPascalFileChange.Remove(TNyxPascalSourceRef.Main);
  except
    on ENyxPascalProject do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'inventory authoring cannot remove main source');
  LWire := WithNyxSourceProjectFiles(NyxObject([NyxField('source', NyxData(CMain))]),
    TNyxPascalProjectFiles.Capture(LUnit));
  LWire := WithNyxSourceFileChange(LWire, LChange);
  LRead := ReadNyxSourceFileChange(LWire);
  Check(LRead.IsDefined and (LRead.Kind = npfcCreate) and
    LRead.FileReference.SameFile(LChange.FileReference) and (LRead.Text = LChange.Text),
    'private creation reconstructs only its named captured member');
  LWire := WithNyxSourceFileChange(NyxObject([NyxField('source', NyxData(CMain))]),
    TNyxPascalFileChange.Remove(LChange.FileReference));
  LRead := ReadNyxSourceFileChange(LWire);
  Check(LRead.IsDefined and (LRead.Kind = npfcRemove) and (LRead.Text = ''),
    'last-helper removal needs no proposed member or duplicated source text');
  LWire := WithNyxSourceFile(NyxObject([NyxField('source', NyxData(CMain))]),
    LChange.FileReference);
  Check((LWire.Field('sourceFile').Count = 2) and not ReadNyxSourceFileChange(LWire).IsDefined,
    'legacy selected-file Apply keeps its exact two-member intent');
  LState := Default(TNyxPascalFilesEditorState);
  LForm := NewNyxPascalFilesEditor('files-form', LState,
    TNyxPascalSourceRef.Main, TNyxPascalSourceRef.Main);
  LForm.Node.Find('files-form-name').Configure.Value('../unfinished').Done;
  LState.Capture('files-form', LForm.Node);
  LRejected := False;
  try
    LState.Creation;
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LState.Name = '../unfinished'),
    'invalid names remain copied form values after explicit Create refusal');
  LForm.Node.Find('files-form-name').Configure.Value('files.helpers').Done;
  LState.Capture('files-form', LForm.Node);
  LCopy := LState.Creation;
  Check(LCopy.FileReference.SameFile(LChange.FileReference),
    'public form supplies the same strong unit reference as code-first callers');
  LForm.Node.Find('files-form-name').Configure.Value('files.later').Done;
  LState.Capture('files-form', LForm.Node);
  Check(not LState.AcceptCreation(LCopy) and (LState.Name = 'files.later'),
    'a delayed compiler completion cannot clear newer form input');
  LForm := NewNyxPascalFilesEditor('files-form', LState,
    LChange.FileReference, LChange.FileReference);
  Check((Pos(LChange.FileReference.Name, LForm.Node.Find('files-form-warning').Prop('text')) > 0) and
    (LForm.Node.Find('files-form-remove-confirm') <> nil),
    'the reusable confirmation names the exact reviewed target');
  LForm := nil;
  Result := LChecks;
end;

function RunNyxPascalProjectEditChecks: Integer;
const
  CMain: TNyxText = 'unit ranges.views;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface' + #10 +
    'implementation' + #10 + 'end.' + #10;
  CInclude: TNyxText = '{ head 🚀 𐐷 ' + #0 + ' tail }' + #10;
  CExpectedInclude: TNyxText = '{ start 🚀 𐐷 ' + #0 + ' finish }' + #10;
var
  LOriginal: INyxPascalProject;
  LResult: INyxPascalProject;
  LGroup: INyxSourceProjectPatch;
  LEdits: array of TNyxSourceFileEdit;
  LMain: TNyxPascalSourceRef;
  LUnit: TNyxPascalSourceRef;
  LInclude: TNyxPascalSourceRef;
  LBefore: TNyxText;
  LUnitText: TNyxText;
  LLarge: TNyxText;
  LCreated: INyxPascalProject;
  LChanges: array of TNyxPascalFileChange;
  LDirective: TNyxText;
  LRejected: Boolean;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

  function Range(const AFile: TNyxPascalSourceRef; AOffset: Integer;
    const AExpected, AReplacement: TNyxText): TNyxSourceFileEdit;
  begin
    Result := NyxSourceFileEdit(AFile, NyxSourceEdit(AOffset, AExpected, AReplacement));
  end;

begin
  LChecks := 0;
  LMain := TNyxPascalSourceRef.Main;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('ranges.helpers'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('notes.inc'));
  LUnitText := TNyxPascalFileChange.CreateUnit(NyxPascalUnit('ranges.helpers')).Text;
  LOriginal := NewNyxPascalProject(NyxPascalUnit('ranges.views'), CMain,
    TNyxPascalProjectLimits.Defaults.UpToFiles(8).UpToBytes(8192))
    .WithUnit(NyxPascalUnit('ranges.helpers'), LUnitText)
    .WithInclude(NyxPascalInclude('notes.inc'), CInclude);
  LBefore := TNyxPascalProjectFiles.Capture(LOriginal).Encoded;
  SetLength(LEdits, 4);
  LEdits[0] := Range(LUnit, NyxTextScalarCount(LUnitText), '', '{ A helper note. }');
  LEdits[1] := Range(LInclude, NyxTextScalarCount(Copy(CInclude, 1, Pos('head', CInclude) - 1)),
    'head', 'start');
  LEdits[2] := Range(LMain, NyxTextScalarCount(CMain), '', '{ A main note. }');
  LEdits[3] := Range(LInclude, NyxTextScalarCount(Copy(CInclude, 1, Pos('tail', CInclude) - 1)),
    'tail', 'finish');
  LGroup := NyxSourceProjectPatch(LEdits);
  LResult := LGroup.PrepareProject(LOriginal);
  Check((LResult.Source = CMain + '{ A main note. }') and
    (LUnit.Text(LResult) = LUnitText + '{ A helper note. }'),
    'one immutable group changes main and unit without altering untouched text');
  Check(LInclude.Text(LResult) = CExpectedInclude,
    'interleaved helper ranges retain supplementary Unicode and embedded NUL');
  Check((LResult.Count = LOriginal.Count) and
    (LResult.Input(0).Name = LOriginal.Input(0).Name) and
    (LResult.Input(1).Name = LOriginal.Input(1).Name) and
    (LResult.Limits.MaximumFiles = 8) and (LResult.Limits.MaximumBytes = 8192),
    'range preparation retains authored inventory order, identity and tuned limits');
  Check((LOriginal.Source = CMain) and
    (TNyxPascalProjectFiles.Capture(LOriginal).Encoded = LBefore),
    'complete candidate preparation never mutates its accepted provider');
  LEdits[0] := Range(LMain, 0, 'wrong', 'replacement');
  Check((LGroup.PrepareProject(LOriginal).Source = LResult.Source) and
    (TNyxPascalProjectFiles.Capture(LGroup.PrepareProject(LOriginal)).Encoded =
      TNyxPascalProjectFiles.Capture(LResult).Encoded),
    'later caller-array mutations cannot retarget the captured group');
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(LInclude, 2, 'wrong', 'replacement')]).PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'mismatched expected text refuses before any accepted publication');
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(LInclude, 4, 'ad', 'x'),
      Range(LMain, 0, '', ' '), Range(LInclude, 3, 'e', 'y')]).PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'interleaving does not permit out-of-order ranges in the same file');
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(LInclude, 2, 'head', 'x'),
      Range(LMain, 0, '', ' '), Range(LInclude, 3, 'ead', 'y')]).PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'overlapping same-file ranges refuse across intervening files');
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(TNyxPascalSourceRef.IncludeFile(
      NyxPascalInclude('NOTES.inc')), 0, '', '{ A note. }')]).PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a case alias cannot select or create an accepted helper');
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(TNyxPascalSourceRef.UnitFile(
      NyxPascalUnit('ranges.absent')), 0, '', ' ')]).PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'missing files refuse without silently selecting main or adding membership');
  LLarge := StringOfChar('x', 131073);
  LRejected := False;
  try
    NyxSourceProjectPatch([Range(LMain, 0, '', LLarge), Range(LInclude, 0, '', LLarge)]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'replacement budgets apply across the whole project group');
  LRejected := False;
  try
    NyxSourceProjectPatch([]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an empty project range group refuses');
  LRejected := False;
  try
    SetLength(LEdits, 17);
    NyxSourceProjectPatch(LEdits);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'more than sixteen changes refuse before inspecting partial ranges');
  Check((LOriginal.Source = CMain) and
    (TNyxPascalProjectFiles.Capture(LOriginal).Encoded = LBefore),
    'all refusal cases retain the exact original project');
  LDirective := 'implementation' + #10 + '{$I additions.inc}';
  SetLength(LChanges, 1);
  LChanges[0] := TNyxPascalFileChange.CreateInclude(NyxPascalInclude('additions.inc'))
    .WithText(CInclude);
  LGroup := NyxSourceProjectMutation(LChanges, [Range(LMain,
    NyxTextScalarCount(Copy(CMain, 1, Pos('implementation', CMain) - 1)),
    'implementation', LDirective)]);
  LChanges[0] := TNyxPascalFileChange.CreateInclude(NyxPascalInclude('different.inc'));
  LCreated := LGroup.PrepareProject(LOriginal);
  Check((LCreated.Count = LOriginal.Count + 1) and
    (TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('additions.inc')).Text(LCreated) = CInclude),
    'captured creation and original-source directive admit together with exact Unicode/NUL');
  Check(Pos(LDirective, LCreated.Source) > 0,
    'related dependency text is prepared before complete project validation');
  LRejected := False;
  try
    TNyxPascalFileChange.Remove(TNyxPascalSourceRef.IncludeFile(
      NyxPascalInclude('additions.inc'))).Candidate(LCreated);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'ordinary single-file removal retains missing-include refusal');
  LResult := NyxSourceProjectMutation([TNyxPascalFileChange.Remove(
    TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('additions.inc')))],
    [Range(LMain, NyxTextScalarCount(Copy(LCreated.Source, 1,
      Pos(LDirective, LCreated.Source) - 1)), LDirective, 'implementation')])
    .PrepareProject(LCreated);
  Check((LResult.Source = CMain) and
    (TNyxPascalProjectFiles.Capture(LResult).Encoded = LBefore),
    'grouped removal repairs its dependency and returns every original input exactly');
  LRejected := False;
  try
    NyxSourceProjectMutation([TNyxPascalFileChange.Remove(LInclude)],
      [Range(LInclude, 0, '', ' ')]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'membership owns a whole file and cannot ambiguously share its ranges');
  LRejected := False;
  try
    NyxSourceProjectMutation([TNyxPascalFileChange.CreateInclude(NyxPascalInclude('NOTES.inc'))], [])
      .PrepareProject(LOriginal);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'new file creation refuses a case alias of an existing helper');
  LRejected := False;
  try
    NyxPascalFileChanges([TNyxPascalFileChange.Remove(LInclude),
      TNyxPascalFileChange.CreateInclude(NyxPascalInclude('notes.inc'))]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a membership target cannot appear twice in one reviewed group');
  LRejected := False;
  try
    NyxSourceProjectMutation([TNyxPascalFileChange.CreateInclude(
      NyxPascalInclude('large.inc')).WithText(LLarge)], [Range(LMain, 0, '', LLarge)]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'new file bytes and source replacements share the total scalar budget');
  LRejected := False;
  try
    NyxSourceProjectMutation([], [Range(LMain, 0, '', ' ')]);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'range-only proposals use the existing fixed-inventory contract');
  LRejected := False;
  try
    NyxSourceProjectMutation([TNyxPascalFileChange.Remove(TNyxPascalSourceRef.Main)], []);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'no grouped path can remove the main source');
  Check((LOriginal.Source = CMain) and
    (TNyxPascalProjectFiles.Capture(LOriginal).Encoded = LBefore) and
    (Pos(LDirective, LCreated.Source) > 0),
    'successful and refused groups retain both independent previous projects');
  Result := LChecks;
end;

function RunNyxPascalRoutineIndexChecks: Integer;
const
  CMain: TNyxText = 'unit browse.main;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface' + #10 + 'implementation' + #10 + 'end.' + #10;
  CHelper: TNyxText = 'unit browse.helpers;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface' + #10 + 'uses nyx.text;' + #10 + '{$I api.inc}' + #10 +
    'implementation' + #10 + '(*$include ''bodies.inc''*)' + #10 + 'end.' + #10;
  CAPI: TNyxText = 'function Plan: TNyxText;' + #10;
  CBody: TNyxText = '{ A physical Unicode prefix 🚀 𐐷 }' + #10 +
    'function Plan: TNyxText;' + #10 +
    '  function Nested: Integer;' + #10 + '  begin' + #10 + '    Result := 1;' + #10 + '  end;' + #10 +
    'begin' + #10 + '  {$I value.inc}' + #10 + 'end;' + #10 +
    'procedure Other;' + #10 + 'begin' + #10 + 'end;' + #10;
  CValue: TNyxText = 'Result := ''Original 🚀 {$I absent.inc} function Phantom: Integer;'';' + #10;
  CPlan: TNyxText = #10 + 'begin' + #10 + '  Result := TNyxText(''Updated 🚀 𐐷'');' + #10 + 'end;';
var
  LProject: INyxPascalProject;
  LChanged: INyxPascalProject;
  LVariant: INyxPascalProject;
  LIndex: INyxPascalRoutineIndex;
  LBeforeIndex: INyxPascalRoutineIndex;
  LSite: TNyxPascalRoutineSite;
  LOther: TNyxPascalRoutineSite;
  LSession: TNyxStudioSession;
  LOwner: TNyxPascalSourceRef;
  LBody: TNyxPascalSourceRef;
  LAPI: TNyxPascalSourceRef;
  LValue: TNyxPascalSourceRef;
  LPatch: INyxSourceProjectPatch;
  LBefore: TNyxText;
  LRejected: Boolean;
  LChecks: Integer;
  LKind: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LOwner := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('browse.helpers'));
  LBody := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('bodies.inc'));
  LAPI := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('api.inc'));
  LValue := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('value.inc'));
  LProject := NewNyxPascalProject(NyxPascalUnit('browse.main'), CMain)
    .WithUnit(NyxPascalUnit('browse.helpers'), CHelper)
    .WithInclude(NyxPascalInclude('api.inc'), CAPI)
    .WithInclude(NyxPascalInclude('bodies.inc'), CBody)
    .WithInclude(NyxPascalInclude('value.inc'), CValue);
  LBefore := TNyxPascalProjectFiles.Capture(LProject).Encoded;
  LIndex := CaptureNyxPascalRoutineIndex(LProject, LOwner);
  Check((LIndex.Count = 2) and LIndex.Owner.SameFile(LOwner),
    'one owning helper indexes include definitions without promoting nested or quoted routines');
  LSite := LIndex.Item(0);
  LOther := LIndex.Item(1);
  Check((LSite.Routine.Name = 'Plan') and (LOther.Routine.Name = 'Other'),
    'expanded discovery retains physical declaration order');
  Check(LSite.Definition.FileReference.SameFile(LBody) and
    (LSite.Definition.Line = 2) and (LSite.Definition.Column = 1) and
    (LSite.Definition.Offset = NyxTextScalarCount(Copy(CBody, 1, Pos('function Plan', CBody) - 1))),
    'physical definition uses Unicode scalars rather than expanded offsets or UTF-8 bytes');
  Check(LSite.SignatureLocation.IsDefined and LSite.BodyLocation.IsDefined and
    LSite.BodyLocation.FileReference.SameFile(LBody) and LSite.Editable,
    'a complete include-owned body has an exact safe physical range');
  Check((Pos('{$I value.inc}', LSite.Body) > 0) and (Pos('Original', LSite.Body) = 0) and
    (Pos('Nested', LSite.Body) > 0),
    'contiguous body windows retain authored directives and nested routines, not expanded replacements');
  Check((LSite.Visibility = prvInterface) and (LSite.Declaration = Copy(CAPI, 1, Length(CAPI) - 1)) and
    LSite.DeclarationLocation.FileReference.SameFile(LAPI) and (LSite.DeclarationLocation.Line = 1),
    'counterpart declarations retain their own physical include and exact signature');
  Check(LSite.TouchesFile(LBody) and LSite.TouchesFile(LAPI) and LSite.TouchesFile(LValue),
    'all contributing files remain queryable, including nested body includes');
  Check(CaptureNyxPascalRoutineIndex(LProject, TNyxPascalSourceRef.Main).Count = 0,
    'ordinary units need no synthetic Nyx managed-view markers');
  LSession := TNyxStudioSession.Create;
  try
    LVariant := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(LSession.Source)),
      LSession.Source);
    LIndex := CaptureNyxPascalRoutineIndex(LVariant, TNyxPascalSourceRef.Main);
    Check(LIndex.Count > 0, 'managed main browsing retains readable compiler infrastructure');
    LRejected := False;
    for LKind := 0 to LIndex.Count - 1 do
    begin

      if SameText(LIndex.Item(LKind).Routine.Name, 'BuildNyxDocument') then
      begin
        LRejected := not LIndex.Item(LKind).Editable;
      end;
    end;
    Check(LRejected, 'managed builders remain noneditable through project routine context');
  finally
    LSession.Free;
  end;
  LIndex := CaptureNyxPascalRoutineIndex(LProject, LOwner);
  LBeforeIndex := LIndex;
  LPatch := NewNyxProjectRoutinePatch([
    NyxProjectRoutineEdit(LOwner, NyxRoutineEdit(LOther.Routine, LOther.Body,
      #10 + 'begin' + #10 + '  { An independent sibling note. }' + #10 + 'end;')),
    NyxProjectRoutineEdit(LOwner, NyxRoutineEdit(LSite.Routine, LSite.Body, CPlan))]);
  LChanged := LPatch.PrepareProject(LProject);
  LIndex := CaptureNyxPascalRoutineIndex(LChanged, LOwner);
  Check((LChanged.Source = CMain) and (LOwner.Text(LChanged) = CHelper) and
    (LAPI.Text(LChanged) = CAPI) and (LValue.Text(LChanged) = CValue),
    'reverse-ordered semantic bodies retain main, unit, counterparts and unused nested include bytes');
  Check((LIndex.Item(0).Body = CPlan) and
    (Pos('independent sibling note', LIndex.Item(1).Body) > 0),
    'typed body preparation sorts original physical ranges and changes both exact routines');
  Check((TNyxPascalProjectFiles.Capture(LProject).Encoded = LBefore) and
    (LBeforeIndex.Item(0).Body = LSite.Body),
    'old input and detached index values survive independent candidate preparation');
  for LKind := 0 to 2 do
  begin
    LRejected := False;
    try
      case LKind of
        0:
          LPatch := NewNyxProjectRoutinePatch([NyxProjectRoutineEdit(LOwner,
            NyxRoutineEdit(LSite.Routine, 'wrong', CPlan))]);
        1:
          LPatch := NewNyxProjectRoutinePatch([NyxProjectRoutineEdit(LOwner,
            NyxRoutineEdit(LSite.Routine, LSite.Body, CPlan + #10 +
              'procedure Escaped; begin end;'))]);
        2:
          LPatch := NewNyxProjectRoutinePatch([NyxProjectRoutineEdit(LOwner,
            NyxRoutineEdit(LSite.Routine, LSite.Body, CPlan + #10 + 'var Escaped: Integer;'))]);
      end;
      LPatch.PrepareProject(LProject);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxPascalProjectFiles.Capture(LProject).Encoded = LBefore),
      'mismatched expectations and sibling/declaration escapes preserve the original inputs');
  end;
  for LKind := 0 to 3 do
  begin
    LRejected := False;
    try
      case LKind of
        0:
          CaptureNyxPascalRoutineIndex(LProject, LBody);
        1:
          CaptureNyxPascalRoutineIndex(LProject, LOwner,
            TNyxPascalRoutineLimits.Defaults.UpToExpandedBytes(16));
        2:
          CaptureNyxPascalRoutineIndex(LProject, LOwner,
            TNyxPascalRoutineLimits.Defaults.UpToDepth(1));
        3:
          CaptureNyxPascalRoutineIndex(LProject, LOwner,
            TNyxPascalRoutineLimits.Defaults.UpToIncludes(1));
      end;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unowned fragments and explicit browse budgets refuse without a partial index');
  end;
  LVariant := LProject.WithUnit(NyxPascalUnit('browse.helpers'),
    'unit browse.helpers; interface implementation {$I head.inc}{$I tail.inc} end.')
    .WithInclude(NyxPascalInclude('head.inc'), 'function Split: Integer;' + #10 + 'begin' + #10)
    .WithInclude(NyxPascalInclude('tail.inc'), 'Result := 7;' + #10 + 'end;');
  LIndex := CaptureNyxPascalRoutineIndex(LVariant, LOwner);
  Check((LIndex.Count = 1) and not LIndex.Item(0).Editable and
    not LIndex.Item(0).BodyLocation.IsDefined and (Pos('Result := 7', LIndex.Item(0).Body) > 0) and
    LIndex.Item(0).TouchesFile(TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('tail.inc'))),
    'physically split implementations stay readable and discoverable without a fabricated edit site');
  LVariant := LProject.WithUnit(NyxPascalUnit('browse.helpers'),
    'unit browse.helpers; interface implementation {$I cycle-a.inc} end.')
    .WithInclude(NyxPascalInclude('cycle-a.inc'), '{$I cycle-b.inc}')
    .WithInclude(NyxPascalInclude('cycle-b.inc'), '{$I cycle-a.inc}');
  LRejected := False;
  try
    CaptureNyxPascalRoutineIndex(LVariant, LOwner);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'include cycles refuse before returning any indexed context');
  LVariant := LProject.WithUnit(NyxPascalUnit('browse.helpers'),
    'unit browse.helpers; interface implementation ' +
    '{$ifdef FIRST}procedure Conditional; begin end;{$endif} end.');
  LIndex := CaptureNyxPascalRoutineIndex(LVariant, LOwner);
  Check((LIndex.Count = 1) and not LIndex.Item(0).Editable and (LIndex.Item(0).Reason <> ''),
    'conditional definitions retain discovery and explicit focused-edit refusal');
  LVariant := LProject.WithInclude(NyxPascalInclude('api.inc'), 'function Plan: Integer;');
  LIndex := CaptureNyxPascalRoutineIndex(LVariant, LOwner);
  Check((LIndex.Item(0).Visibility = prvUnknown) and not LIndex.Item(0).Editable,
    'incompatible declaration counterparts cannot pretend to own an editable signature');
  LVariant := LProject.WithUnit(NyxPascalUnit('browse.helpers'),
    'unit browse.helpers; interface implementation ' +
    'function Convention: Integer; cdecl;' + #10 + 'begin Result := 7; end; end.');
  LIndex := CaptureNyxPascalRoutineIndex(LVariant, LOwner);
  LOther := LIndex.Item(0);
  Check((Pos('cdecl;', LOther.Signature) > 0) and (Pos('cdecl', LOther.Body) = 0) and LOther.Editable,
    'calling convention belongs to the immutable declaration, never the replaceable body');
  LChanged := NewNyxProjectRoutinePatch([NyxProjectRoutineEdit(LOwner,
    NyxRoutineEdit(LOther.Routine, LOther.Body, #10 + 'begin Result := 9; end;'))])
    .PrepareProject(LVariant);
  LIndex := CaptureNyxPascalRoutineIndex(LChanged, LOwner);
  Check((LIndex.Item(0).Signature = LOther.Signature) and (Pos('Result := 9', LIndex.Item(0).Body) > 0),
    'typed replacement preserves the complete calling-convention signature exactly');
  LRejected := False;
  try
    NewNyxProjectRoutinePatch([NyxProjectRoutineEdit(LOwner,
      NyxRoutineEdit(LOther.Routine, LOther.Body, #10 + 'stdcall;' + #10 + 'begin Result := 9; end;'))])
      .PrepareProject(LVariant);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'a focused body cannot insert a different calling convention before begin');
  LVariant := NewNyxPascalProject(NyxPascalUnit('browse.main'),
    'unit browse.main; interface implementation function BuildNyxDocument: Integer;' +
    'begin Result := 1; end; end.');
  LIndex := CaptureNyxPascalRoutineIndex(LVariant, TNyxPascalSourceRef.Main);
  Check((LIndex.Count = 1) and not LIndex.Item(0).Editable,
    'main compiler entry points stay protected even without a managed-view frame');
  LProject := nil;
  Check((LBeforeIndex.Count = 2) and (LBeforeIndex.Item(0).Declaration = LSite.Declaration),
    'copied source windows and locations outlive the complete provider');
  Result := LChecks;
end;

end.
