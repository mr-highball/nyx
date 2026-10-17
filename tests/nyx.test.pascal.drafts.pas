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

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts;

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

end.
