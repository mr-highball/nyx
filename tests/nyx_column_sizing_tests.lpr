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
program nyx_column_sizing_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.typeahead,
  nyx.collections.columns, nyx.layout.flow,
  nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.studio.session, nyx.studio.collections, nyx.studio.authoring,
  nyx.studio.stateedits, nyx.studio.projects
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LPolicies: TNyxColumnSizings;
  LNatural: TNyxFlowSizes;
  LWidths: TNyxFlowSizes;
  LSpec: TNyxCollectionViewSpec;
  LCopy: TNyxCollectionViewSpec;
  LKey: TNyxCollectionRef;
  LVersion: Integer;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LData: TNyxDataValue;
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LPage: TNyxNode;
  LTable: TNyxNode;
  LWorkspace: TNyxSourceWorkspace;
  LIntent: TNyxStudioCollectionIntent;
  LSession: TNyxStudioSession;
  LPanel: TNyxNode;
  LPatch: INyxCollectionPatch;
  LPair: TNyxProjectPair;
  LNext: TNyxProjectPair;
begin
  SetLength(LPolicies, 3);
  SetLength(LNatural, 3);
  LNatural[0] := 100;
  LNatural[1] := 50;
  LNatural[2] := 50;
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 300) and (LWidths[1] = 150) and (LWidths[2] = 150),
    'default content fills available width proportionally');
  LPolicies[0] := NyxColumnSizing.Flex(3).Minimum(100);
  LPolicies[1] := NyxColumnSizing.Fixed(80);
  LPolicies[2] := NyxColumnSizing.Flex(1).Minimum(50);
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 390) and (LWidths[1] = 80) and (LWidths[2] = 130),
    'mixed fixed and flexible columns share exact available width');
  LWidths := NyxColumnWidths(200, LPolicies, LNatural);
  Check((LWidths[0] = 100) and (LWidths[1] = 80) and (LWidths[2] = 50),
    'admitted minima deliberately overflow a narrow host');
  LBefore := LPolicies[0].ToData.ToJSON;
  LPolicies[0] := LPolicies[0].Maximum(120);
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 120) and (LWidths[1] = 80) and (LWidths[2] = 400),
    'maximum-frozen space redistributes to a flexible sibling');
  Check(TNyxColumnSizing.FromData(TNyxDataValue.ParseJSON(LBefore)).Bounds.MaximumValue = 100000,
    'retained policy bytes are independent from a later fluent change');
  LRejected := False;
  try
    NyxColumnSizing.Fixed(0);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'zero fixed pixels refuse');
  LRejected := False;
  try
    NyxColumnSizing.Flex(-1);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'negative weights refuse');
  LRejected := False;
  try
    LPolicies[0].Maximum(99);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPolicies[0].Bounds.MaximumValue = 120),
    'reversed bounds refuse without mutating the retained value');
  LNatural[1] := 0;
  LRejected := False;
  try
    NyxColumnWidths(600, LPolicies, LNatural);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'invalid host measurement refuses before allocation');
  LNatural[1] := 50;
  LRejected := False;
  try
    TNyxColumnSizing.FromData(TNyxDataValue.ParseJSON(
      '{"version":1,"mode":"other","value":1,"minimum":1,"maximum":100}'));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'unknown persistence mode refuses');
  LKey := NyxCollection('ideas');
  for LVersion := 1 to 4 do
  begin
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority');

    if LVersion > 1 then
    begin
      LSpec := LSpec.Selection(nsmMultiple);
    end;

    if LVersion = 3 then
    begin
      LSpec := LSpec.Query(NyxCollectionQuery.OrderBy(NyxTextField('title')));
    end;

    if LVersion = 4 then
    begin
      LSpec := LSpec.TypeAhead(NyxTypeAhead);
    end;
    LBefore := LSpec.ToData.ToJSON;
    LCopy := TNyxCollectionViewSpec.FromData(TNyxDataValue.ParseJSON(LBefore));
    Check((LCopy.ToData.ToJSON = LBefore) and not LCopy.HasColumnSizing,
      'every legacy descriptor remains byte-compatible');
    LCopy := LSpec.ColumnSizing(NyxColumnSizing.Fixed(96));
    LData := LCopy.ToData;
    Check((LData.Field('version').AsInteger = 5) and
      (TNyxCollectionViewSpec.FromData(LData).ToData.ToJSON = LData.ToJSON),
      'sizing round-trips independently with legacy query/selection/typeahead');
    Check(LSpec.ToData.ToJSON = LBefore, 'copied fluent columns do not mutate the original binding');
    Check(LCopy.UseDefaultColumnSizing.ToData.ToJSON = LBefore,
      'restoring default sizing returns exact legacy bytes');
  end;
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('title'), '')
      .Integer(NyxIntegerField('priority'), 1), [
      NyxCollectionItem(NyxItem(LKey, 'first')).WithValue(NyxTextField('title'), 'Explore an idea')]);
    LPage := TNyxNode.Create(nkPage, 'page');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'ideas-table');
    LPage.Add(LTable);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea', cmEditable)
      .ColumnSizing(NyxColumnSizing.Flex(3).Minimum(120))
      .Column(NyxIntegerField('priority'), 'Priority')
      .ColumnSizing(NyxColumnSizing.Fixed(96));
    LTable.SetCollectionView(LSpec);
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.ColumnSizing(NyxColumnSizing.Flex(3).Minimum(120))', LSource) > 0) and
      (Pos('nyx.collections.columns,', LSource) > 0), 'crafted source uses typed policy and its owning import');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'public source reader reconstructs complete column meaning');
    FreeAndNil(LCandidate);
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := LKey;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('title'));
    LIntent.Projection := cpTable;
    LIntent.Value := 'A clearer idea';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check((LCopy.ColumnAt(0).Sizing.ToData.ToJSON = LSpec.ColumnAt(0).Sizing.ToData.ToJSON) and
      (LCopy.ColumnAt(1).Sizing.ToData.ToJSON = LSpec.ColumnAt(1).Sizing.ToData.ToJSON),
      'ordinary title edits retain every copied width policy');
    LIntent.Action := scaColumnFixed;
    LIntent.Value := '144';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check((LCopy.ColumnAt(0).Sizing.Mode = ncsFixed) and
      (LCopy.ColumnAt(0).Sizing.Value = 144) and (LSpec.ColumnAt(0).Sizing.Mode = ncsFlex),
      'typed ordinary sizing changes one independent column');
    LPair := Default(TNyxProjectPair);
    LPair.Design := LBefore;
    LPair.Source := LSource;
    LPatch := NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('ideas-table'), LIntent)]);
    LData := LPatch.ToData;
    Check(LData.Item(0).Field('value').AsInteger = 144,
      'semantic dimensions are JSON Integers at the explicit wire boundary');
    Check(ReadNyxCollectionPatch(LData).ToData.ToJSON = LData.ToJSON,
      'narrow width intent retains exact typed wire meaning');
    LNext := ReadNyxCollectionPatch(LData).Candidate(LPair);
    LCandidate := TNyxCodec.Decode(LNext.Design);
    Check((LCandidate.Find('ideas-table').CollectionView.ColumnAt(0).Sizing.Value = 144) and
      (Pos('.Fixed(144)', LNext.Source) > 0),
      'semantic width replay admits both design and fluent Pascal');
    FreeAndNil(LCandidate);
    LRejected := False;
    try
      ReadNyxCollectionPatch(TNyxDataValue.ParseJSON(
        '[{"op":"intent","action":"column-fixed","key":"ideas",' +
        '"owner":"ideas-table","projection":"table","field":"title",' +
        '"kind":"text","value":"144"}]'));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'numeric wire strings cannot impersonate typed dimensions');
    LIntent.Value := '0';
    LRejected := False;
    try
      NyxCollectionPatch([NyxCollectionIntentChange(NyxBindingOwner('ideas-table'), LIntent)])
        .Candidate(LPair);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LPair.Design = LBefore) and (LPair.Source = LSource),
      'invalid width admission preserves both accepted halves');
    LIntent.Value := '144';
    LIntent.Projection := cpList;
    LRejected := False;
    try
      LIntent.Validate;
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'table width actions refuse another projection family');
    LIntent.Projection := cpTable;
    LIntent.Action := scaColumnDefault;
    LIntent.Value := '';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LCopy,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check(not LCopy.ColumnAt(0).Sizing.Defined and LCopy.ColumnAt(1).Sizing.Defined,
      'default reset preserves other column choices');
    LSession := TNyxStudioSession.Create;
    LPanel := TNyxNode.Create(nkColumn, 'panel');
    try
      LSession.Load(LBefore);
      LSession.Select('ideas-table');
      AddNyxCollectionBindingPanel(LPanel, LSession, LSession.Document.Find('ideas-table'));
      Check((LPanel.Find('collection-column-0-flex') <> nil) and
        (LPanel.Find('collection-column-1-fixed').Prop('value') = '96'),
        'Nyx-built Bindings panel exposes actual typed sizing values');
    finally
      LPanel.Free;
      LSession.Free;
    end;
    LRejected := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument,
        StringReplace(LSource, '.Fixed(96)', '.Fixed(''96'')', []));
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'raw string dimensions refuse while accepted document meaning stays exact');
    LTable.SetCollectionView(NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea'));
    Check(not LDocument.HasColumnSizing and
      (Pos('nyx.collections.columns,', TNyxCodegen.Generate(LDocument)) = 0),
      'legacy source receives no unnecessary sizing import');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-sizing-tests', 'passed');
    document.body.setAttribute('data-column-sizing-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' column sizing contract/source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-sizing-tests', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
