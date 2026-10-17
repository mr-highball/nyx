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

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.filechanges,
  nyx.pascal.files.editor, nyx.controls, nyx.studio.sourcebuilds;

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

end.
