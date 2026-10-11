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

{ Actual compiler/constructor continuation, in an independent owned editor.
  Typed row/schema/view/resource edits share the existing guarded source queue
  and one paired history command. No live Studio project or listener is needed. }
procedure CollectionJourney(AExecutor: TNyxBuildExecutor; const ACompiler: INyxSourceCompiler;
  const AOriginal: INyxSourceProjection; const AOutput: TNyxText);
const
  CHelper: TNyxText =
    'procedure DefineNotebookRows(ADocument: TNyxDocument);' + #10 +
    'begin' + #10 +
    '  ADocument.Collections.Define(NyxCollection(''roster''),' + #10 +
    '    NyxCollectionSchema.Text(NyxTextField(''label''), TNotebookCards.Caption)' + #10 +
    '      .Integer(NyxIntegerField(''rank''), 1 + 2, NyxIntegerDomain.Range(0, 9))' + #10 +
    '      .Number(NyxNumberField(''weight''), 1 / 8)' + #10 +
    '      .Field(''active'', TNyxStateValue.FromBoolean(True), NyxNoDomain),' + #10 +
    '    [NyxCollectionItem(NyxItem(NyxCollection(''roster''), ''first'')),' + #10 +
    '     NyxCollectionItem(NyxItem(NyxCollection(''roster''), ''second''))' + #10 +
    '       .WithValue(NyxTextField(''label''), PageName(2))' + #10 +
    '       .WithValue(NyxIntegerField(''rank''), 5)]);' + #10 +
    '  ADocument.Collections.Define(NyxCollection(''pins''),' + #10 +
    '    NyxCollectionSchema.Text(NyxTextField(''label''), TNotebookCards.Caption),' + #10 +
    '    [NyxCollectionItem(NyxItem(NyxCollection(''pins''), ''retained''))]);' + #10 +
    'end;' + #10 + #10;
  CChangedCaption: TNyxText = 'A recomputed collection 🚀 𐐷 é.';
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LSource: TNyxText;
  LChangedSource: TNyxText;
  LStages: array of TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
  LBase: INyxSourceProjection;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
  LIntent: TNyxStudioCollectionIntent;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LCopy: TNyxDocument;
  LDefinition: INyxCollectionSnapshot;
  LRows: TNyxResourceRows;
  LRejected: Boolean;
  LPosition: Integer;
  LDraft: TNyxText;
  LEdit: TNyxStudioDesignEdit;

  { Splice exact UTF-8 source in its portable type. Native ANSI RTL replacement
    would transcode supplementary qualification text before project admission. }
  function ReplaceFirst(const AText, AExpected, AReplacement: TNyxText): TNyxText;
  var
    LStart: Integer;
  begin
    LStart := Pos(AExpected, AText);
    Check(LStart > 0, 'owned source splice finds its expected exact text');
    Result := Copy(AText, 1, LStart - 1) + AReplacement +
      Copy(AText, LStart + Length(AExpected), MaxInt);
  end;

  procedure Submit(const ATransaction: INyxProjectTransaction; const AReason: TNyxText);
  var
    LEdit: TNyxStudioDesignEdit;
    LBeforePair: TNyxProjectPair;
    LAfterPair: TNyxProjectPair;
  begin
    LBeforePair := NyxProjectPair(LSession.Save, LSession.Source);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Transaction := CaptureNyxProjectTransaction(ATransaction);
    LCommands.Edit(LEdit);
    Drain(LCommands);
    Check(LCommands.State = nssApplied, AReason + ': ' + LCommands.Message);
    LAfterPair := NyxProjectPair(LSession.Save, LSession.Source);
    SetLength(LStages, Length(LStages) + 1);
    LStages[High(LStages)] := NyxObject([
      NyxField('beforeSource', NyxData(LBeforePair.Source)),
      NyxField('beforeDesign', NyxData(LBeforePair.Design)),
      NyxField('afterSource', NyxData(LAfterPair.Source)),
      NyxField('afterDesign', NyxData(LAfterPair.Design))]);
    Check(not LSession.SourceDraftPending and (LSession.DraftSource = LSession.Source),
      AReason + ' resolves its exact accepted source');
    Check(Pos('DefineNotebookRows(LDocument)', LSession.Source) > 0,
      AReason + ' retains the handwritten collection helper');
    LSession.Undo;
    Check((LSession.Save = LBeforePair.Design) and (LSession.Source = LBeforePair.Source),
      AReason + ' has one exact paired Undo');
    LSession.Redo;
    Check((LSession.Save = LAfterPair.Design) and (LSession.Source = LAfterPair.Source),
      AReason + ' has one exact paired Redo');
  end;

  procedure Collections(const AChanges: array of TNyxCollectionChange; const AReason: TNyxText);
  begin
    Submit(NyxProjectTransaction([NyxCollectionStep(NyxCollectionPatch(AChanges))]), AReason);
  end;

begin
  LSession := nil;
  LCommands := nil;
  LBefore := nil;
  LAfter := nil;
  LCopy := nil;
  try
    LSource := AOriginal.Source;
    LSource := EditNyxImport(LSource, nisImplementation, niaAdd, NyxPascalUnit('nyx.collections'));
    LSource := EditNyxImport(LSource, nisImplementation, niaAdd, NyxPascalUnit('nyx.contract'));
    LPosition := Pos('function BuildNyxDocument: TNyxDocument;' + #10 + 'var', LSource);
    Check(LPosition > 0, 'owned handwritten builder is uniquely identifiable');
    Insert(CHelper, LSource, LPosition);
    LSource := ReplaceFirst(LSource, '    LDocument.AddComponent(TNotebookCards.Definition);',
      '    DefineNotebookRows(LDocument);' + #10 +
      '    LDocument.AddComponent(TNotebookCards.Definition);');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted,
      'actual handwritten collection helper executes: ' + LBuild.Projection.Message);
    LBase := LBuild.Projection;
    SaveText(AOutput + 'web/collection-initial-source.pas', LSource);
    SaveText(AOutput + 'web/collection-initial-design.json', LBase.Design);
    Check(LBase.Source = LSource, 'collection producer retains exact submitted source');
    Check(DecodeNyxProject(EncodeNyxProject(NyxProjectPair(LBase.Design, LSource))).Source = LSource,
      'collection helper retains exact project-wire source');
    Check(DecodeNyxProject(EncodeNyxProject(NyxProjectPair(LBase.Design, LSource))).Design = LBase.Design,
      'collection helper retains exact project-wire design');
    LSession := TNyxStudioSession.Create;
    LSession.AdoptProjectedProject(NyxProjectPair(LBase.Design, LSource), LBase);
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(ACompiler);
    LKey := NyxCollection('roster');
    LSchema := LSession.Document.Collections.Snapshot(LKey).Schema;
    LField := LSchema.Definition(NyxIntegerField('rank'));
    Check(LField.DefaultValue.IntegerValue = 3, 'typed schema metadata reads exact integer defaults');
    Check(NyxCollectionSchema.Field(LSchema.Definition(NyxTextField('label'))).Count = 1,
      'immutable field composition has an independent schema');
    LRejected := False;
    try
      LSchema.Definition(NyxBooleanField('rank'));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed field metadata rejects another scalar family');
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('label'), 'Note', cmEditable)
      .Column(NyxIntegerField('rank'), 'Rank', cmEditable)
      .Column(NyxNumberField('weight'), 'Weight', cmEditable)
      .Column(NyxBooleanField('active'), 'Active', cmEditable);
    Submit(NyxProjectTransaction([
      NyxDesignStep(NyxPlacementPatch([
        NyxPlaceNewControl(nkTable, NyxControl('collection-table'), NyxControl('notebook-1'), nplInside),
        NyxPlaceNewControl(nkList, NyxControl('collection-list'), NyxControl('notebook-1'), nplInside)])),
      NyxCollectionStep(NyxCollectionPatch([
        NyxBindCollection(NyxBindingOwner('collection-table'), cpTable, LSpec),
        NyxBindCollection(NyxBindingOwner('collection-list'), cpList,
          NyxCollectionView(LKey).Column(NyxTextField('label'), 'Note').Selection(nsmMultiple))]))]),
      'construct specialized collection controls and bind them together');
    Collections([NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'first'))
      .WithValue(NyxIntegerField('rank'), 7))], 'edit exactly one cell');
    Check(Pos('.Update(', LSession.Source) > 0, 'cell edit uses an independent partial update');
    Check(LSession.Document.Collections.Snapshot(LKey).Item(NyxItem(LKey, 'first'))
      .GetValue(NyxNumberField('weight')) = 0.125, 'unedited computed numeric cell remains exact');
    Collections([NyxAppendCollectionRow(NyxCollectionItem(NyxItem(LKey, 'third'))
      .WithValue(NyxTextField('label'), 'Third note'))],
      'append a typed row without replacing computed existing rows');
    Collections([NyxMoveCollectionRow(NyxItem(LKey, 'third'), 0)],
      'move a stable row identity to the first position');
    Check(LSession.Document.Collections.Snapshot(LKey).ItemAt(0).Ref.ID = 'third',
      'row order is deliberately retained');
    Collections([NyxSetCollectionField(LKey,
      NyxCollectionSchema.Number(NyxNumberField('weight'), 0.25))],
      'change a schema default while retaining computed metadata and cells');
    Check(Pos('.Schema.Definition(', LSession.Source) > 0,
      'schema continuation reads retained typed field definitions');
    Check(LSession.Document.Collections.Snapshot(LKey).Item(NyxItem(LKey, 'first'))
      .GetValue(NyxNumberField('weight')) = 0.125, 'changing a default retains existing row values');
    Collections([NyxSetCollectionField(LKey,
      NyxCollectionSchema.Boolean(NyxBooleanField('archived'), False))],
      'add a typed field with exact independent defaults');
    Collections([NyxRemoveCollectionField(LKey,
      TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('archived')))],
      'remove only the declared field family');
    Collections([
      NyxSetCollectionQuery(NyxBindingOwner('collection-table'), LKey, cpTable,
        NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('rank')).AtLeast(3))
          .OrderBy(NyxIntegerField('rank'), nsdDescending)),
      NyxSetCollectionTypeAhead(NyxBindingOwner('collection-list'), LKey, cpList,
        NyxTypeAhead.WindowMilliseconds(1600).Match(ntmExact))],
      'configure fluent query and search policies');
    Check((Pos('.Query(', LSession.Source) > 0) and
      (Pos('.TypeAhead(NyxTypeAhead', LSession.Source) > 0),
      'source retains typed query and typeahead objects');
    SaveText(AOutput + 'web/collection-base-source.pas', LSession.Source);
    SaveText(AOutput + 'web/collection-base-design.json', LSession.Save);
    LRows := NyxResourceRows(NyxResourceRef('collection-data')).Field('rows')
      .Identity(NyxResourcePath.Field('id'))
      .Text(NyxTextField('label'), NyxResourcePath.Field('alternate'))
      .Integer(NyxIntegerField('rank'));
    Submit(NyxProjectTransaction([
      NyxResourceStep(NyxResourcePatch([
        NyxDefineResource(NyxResourceRef('collection-data'), NyxDefaultLocale,
          NyxJSONResource('{"rows":[{"id":"packed","label":"Original label",' +
            '"alternate":"A resource-backed row 🚀","rank":2}]}')),
        NyxDefineResourceRows(NyxCollection('catalog'), LRows)])),
      NyxCollectionStep(NyxCollectionPatch([
        NyxBindCollection(NyxBindingOwner('collection-table'), cpTable,
          NyxCollectionView(NyxCollection('catalog'))
            .Column(NyxTextField('label'), 'Packed note')
            .Column(NyxIntegerField('rank'), 'Rank'))]))]),
      'create a resource collection recipe and retarget the existing table');
    Check(LSession.Document.ResourceCollections.HasSource(NyxCollection('catalog')),
      'authored recipe remains separate from its empty schema seed');
    LSource := LSession.Source;
    SaveText(AOutput + 'web/collection-source.pas', LSource);
    SaveText(AOutput + 'web/collection-design.json', LSession.Save);
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btBrowser, spcChecked);
    Check(LBuild.Projection.State = spsCompiled, 'complete collection continuation compiles for pas2js');
    SaveText(AOutput + 'web/collection-worker.json', NyxObject([
      NyxField('reference', NyxData(LBuild.Reference.Name)),
      NyxField('artifact', NyxData(LBuild.Artifact))]).ToJSON);
    LChangedSource := ReplaceFirst(LSource,
      '''A rocket 🚀, a letter 𐐷, and é.''', TNyxCodegen.TextLiteral(CChangedCaption));
    LBuild := AExecutor.ProjectSource(LChangedSource,
      NyxPascalUnit('nyx.projection.fixture'), btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'changed untouched helper compiles and executes');
    LCopy := LBuild.Projection.CopyDocument;
    Check(LCopy.Collections.Snapshot(LKey).Item(NyxItem(LKey, 'first'))
      .GetValue(NyxTextField('label')) = CChangedCaption,
      'row and schema continuations preserve future helper recomputation in the edited collection');
    Check(LCopy.Collections.Snapshot(NyxCollection('pins')).ItemAt(0)
      .GetValue(NyxTextField('label')) = CChangedCaption,
      'unmodified collection also recomputes from its original helper');
    Check(LCopy.Collections.Snapshot(LKey).Item(NyxItem(LKey, 'first'))
      .GetValue(NyxIntegerField('rank')) = 7, 'explicit edited cell remains deliberate after recomputation');
    SaveText(AOutput + 'web/collection-changed-source.pas', LChangedSource);
    SaveText(AOutput + 'web/collection-changed-design.json', LBuild.Projection.Design);
    LBuild := AExecutor.ProjectSource(LChangedSource, NyxPascalUnit('nyx.projection.fixture'),
      btBrowser, spcChecked);
    Check(LBuild.Projection.State = spsCompiled, 'recomputed collection helper compiles for pas2js');
    SaveText(AOutput + 'web/collection-changed-worker.json', NyxObject([
      NyxField('reference', NyxData(LBuild.Reference.Name)),
      NyxField('artifact', NyxData(LBuild.Artifact))]).ToJSON);
    Submit(NyxProjectTransaction([NyxResourceStep(NyxResourcePatch([
      NyxDetachResourceRows(NyxCollection('catalog'))]))]),
      'detach a recipe into exact independent static rows');
    Check(not LSession.Document.ResourceCollections.HasSource(NyxCollection('catalog')) and
      (LSession.Document.Collections.Snapshot(NyxCollection('catalog')).Count = 1),
      'recipe detachment preserves materialized defaults and removes only the source relationship');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaInherit;
    LIntent.Key := NyxCollection('catalog');
    LIntent.Projection := cpTable;
    Collections([NyxCollectionIntentChange(NyxBindingOwner('collection-table'), LIntent)],
      'restore collection-view inheritance');
    Check(not LSession.Document.Find('collection-table').HasCollectionView,
      'inheritance is distinct from an explicit clear');
    Collections([NyxBindCollection(NyxBindingOwner('collection-table'), cpTable,
      NyxCollectionView(NyxCollection('catalog')).Column(NyxTextField('label'), 'Packed note'))],
      'recreate an independently owned local view');
    LIntent.Action := scaClear;
    Collections([NyxCollectionIntentChange(NyxBindingOwner('collection-table'), LIntent)],
      'explicitly clear a collection view');
    Check(LSession.Document.Find('collection-table').HasCollectionView and
      not LSession.Document.Find('collection-table').CollectionView.Defined,
      'explicit clear retains its inherited mask');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaRemove;
    LIntent.Key := NyxCollection('catalog');
    Collections([NyxCollectionIntentChange(LIntent)], 'remove a repaired collection definition');
    SaveText(AOutput + 'web/collection-steps.json', NyxArray(LStages).ToJSON);
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    LDefinition := LAfter.Collections.Snapshot(LKey);
    LAfter.Collections.Remove(LKey);
    LAfter.Collections.Define(LDefinition);
    LRejected := False;
    try
      CustomizeNyxExecutedSource(LSession.Source, LBefore, LAfter);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unexpressed collection-registry reordering refuses the entire proposal');
    LDraft := LSession.Source + #10 + '{ An unfinished collection idea }';
    LSession.SetSourceDraft(LDraft);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
      NyxCollectionStep(NyxCollectionPatch([NyxUpdateCollectionRow(
        NyxCollectionItem(NyxItem(LKey, 'first')).WithValue(NyxIntegerField('rank'), 8))]))]));
    LRejected := False;
    try
      LCommands.Edit(LEdit);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check((LRejected or (LCommands.State = nssRejected)) and
      (LSession.DraftSource = LDraft) and not LCommands.Busy,
      'pending source refuses collection dispatch while retaining the unfinished buffer');
    LSession.DiscardSourceDraft;
  finally
    LCopy.Free;
    LAfter.Free;
    LBefore.Free;
    LCommands.Free;
    LSession.Free;
  end;
end;
