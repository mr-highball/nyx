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
program nyx_collection_inspection_studio_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract,
  nyx.data, nyx.codec, nyx.codegen, nyx.collections, nyx.collections.inspection,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.query.editor,
  nyx.studio.projects, nyx.studio.session, nyx.studio.authoring,
  nyx.studio.collectionintent, nyx.studio.collectionedits, nyx.studio.collections,
  nyx.studio.stateedits, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Inspection Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

{ A saved independent project includes a reusable bound List and a peer. Its
  exact supplementary/NUL text must survive source/design/history operations. }
function Seed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LRecipe: INyxColumn;
  LList: INyxList;
  LInstance: INyxComponent;
  LTree: INyxTree;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
begin
  LDocument := TNyxDocument.Create;
  try
    LKey := NyxCollection('readers');
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
      NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'),
        TNyxText('Original 🌙') + #0 + #13#10 + 'still here')]);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name');
    LPage := NewNyxPage('home');
    LList := NewNyxList('reader-list');
    LList.Binds.Collection(LSpec).Done;
    LPage.Add(LList);
    LTree := NewNyxTree('reader-tree');
    LTree.Binds.Collection(LSpec).Done;
    LPage.Add(LTree);
    LRecipe := NewNyxColumn('reading-panel');
    LList := NewNyxList('definition-list');
    LList.Configure.PartName(NyxPart('records')).Done;
    LList.Binds.Collection(LSpec.Inspection(NyxInspectionPolicy)).Done;
    LRecipe.Add(LList);
    LDocument.AddComponent(LRecipe);
    LInstance := NewNyxComponent('first-panel');
    LInstance.Configure.Component(NyxComponent('reading-panel')).Done;
    LInstance.OverridePart(NyxPart('records'), noProperties).Named('first-records');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-panel');
    LInstance.Configure.Component(NyxComponent('reading-panel')).Done;
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxText('{ Handwritten companion 🌙 }') + #10 + TNyxCodegen.Generate(LDocument));
  finally
    LInstance := nil;
    LTree := nil;
    LList := nil;
    LRecipe := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

{ Form capture is distinct from physical host input. This journey qualifies
  ordinary typed controls, stale refusal and exact paired history/admission. }
procedure Inspector;
var
  LSession: TNyxStudioSession;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioCollectionIntent;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LRefused: Boolean;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := TNyxNode.Create(nkPage, 'inspection-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LShell, LSession, LProjection);
    finally
      FreeAndNil(LProjection);
    end;
  end;

  function Capture(AReset: Boolean = False): Boolean;
  var
    LID: TNyxText;
  begin
    LID := NyxStudioInspectionApplyID;

    if AReset then
    begin
      LID := NyxStudioInspectionDefaultID;
    end;
    Result := CaptureNyxStudioLayout(LSession, LShell.Find(LID), LShell,
      Default(TNyxStudioPendingDesign), LEdit);
  end;

begin
  LSession := TNyxStudioSession.Create(Seed);
  LShell := nil;
  try
    LSession.Select('reader-list');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Rebuild;
    Check((LShell.Find(NyxStudioInspectionEditorID).Count = 7) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'opening a complete independent form preserves absent defaults and source');
    LShell.Find(NyxStudioInspectionFieldID(sifFollow)).Configure.Value('Follow last visible').Done;
    LShell.Find(NyxStudioInspectionFieldID(sifPausedBytes)).Configure.Value('2147483647').Done;
    LShell.Find(NyxStudioInspectionFieldID(sifRemovedIdentities)).Configure.Value('0').Done;
    Check(Capture and (LEdit.Collection.Action = scaInspection) and
      (LEdit.Collection.Inspection.MaximumPausedBytes = High(Integer)),
      'ordinary form captures exact maximum/zero/closed follow choices');
    LIntent := TNyxStudioCollectionIntent.FromData(LEdit.Collection.ToData);
    Check(LIntent.SameIntent(LEdit.Collection), 'private form descriptor retains baseline and typed values');
    Check(ReadNyxCollectionPatch(NyxCollectionPatch([NyxCollectionIntentChange(
      NyxBindingOwner('reader-list'), LIntent)]).ToData).Count = 1,
      'semantic intent descriptor reconstructs the same complete form action');
    LSession.ApplyCollectionIntent(LIntent);
    LApplied := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LSession.Document.Find('reader-list').CollectionView.InspectionPolicy.DefaultFollow = ncfLastVisible) and
      (Pos('.Inspection(NyxInspectionPolicy', LSession.Source) > 0),
      'one ordinary admission pairs saved defaults and crafted Pascal');
    LRefused := False;
    try
      Capture;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LApplied),
      'a stale mounted form cannot overwrite the new binding');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'Undo restores the exact source/design pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LApplied, 'Redo restores the exact authored policy');
    Rebuild;
    LShell.Find(NyxStudioInspectionFieldID(sifPausedBytes)).Configure.Value('unfinished').Done;
    Check(Capture(True) and (LEdit.Collection.Action = scaDefaultInspection),
      'reset ignores unfinished inactive scalar drafts');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Check(not LSession.Document.Find('reader-list').CollectionView.InspectionPolicy.Defined,
      'reset clears only raw inspection presence');
    Rebuild;
    LShell.Find(NyxStudioInspectionFieldID(sifPausedBytes)).Configure.Value('-1').Done;
    LRefused := False;
    try
      Capture;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'negative limits refuse before history admission');
    LSession.Select('reader-tree');
    Rebuild;
    Check(LShell.Find(NyxStudioInspectionFieldID(sifFollow)).Prop('items') = 'Keep reading position',
      'Tree advertises keep-position without unsupported follow-last');
    LShell.Find(NyxStudioInspectionFieldID(sifFollow)).Configure.Value('Follow last visible').Done;
    LRefused := False;
    try
      Capture;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'forged unsupported Tree follow refuses');
    LSession.SetSourceDraft(LSession.Source + #10 + '{ pending handwritten work }');
    LRefused := False;
    try
      Capture(True);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'pending Pascal blocks inspection reset without discarding the draft');
  finally
    LShell.Free;
    LSession.Free;
  end;
end;

{ Exercise the actual semantic dispatcher rather than a mocked patch model.
  Ordered partial edits, expected revisions and one paired Undo own publication. }
procedure Semantic;
var
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LBefore: TNyxText;
  LApplied: TNyxText;
  LRequest: TNyxDataValue;
  LReply: TNyxDataValue;
  LIntent: TNyxStudioCollectionIntent;
  LRefused: Boolean;
  LDocument: TNyxDocument;

  function Pair: TNyxText;
  begin
    Result := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
  end;

begin
  LAgent := TNyxAgentSession.Create(Seed);
  try
    LRevision := LAgent.Revision;
    LBefore := Pair;
    LReply := LAgent.Call('nyx_collections', 'Inspection test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('reader-list')),
      NyxField('limit', NyxData(1))]));
    Check(not LReply.Field('effective').Field('inspection').Field('declared').AsBoolean and
      (Pair = LBefore) and (LAgent.Revision = LRevision), 'bounded inspection read preserves absence/revision');
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := NyxCollection('readers');
    LIntent.Projection := cpList;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
    LIntent.Value := 'Reader 🌙';
    LRequest := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('inspection-group')),
      NyxField('changes', NyxCollectionPatch([
        NyxCollectionIntentChange(NyxBindingOwner('reader-list'), LIntent),
        NyxSetCollectionInspection(NyxBindingOwner('reader-list'), NyxCollection('readers'), cpList,
          NyxInspectionPolicy.PausedBytes(1048576).RemovedIdentities(0).Follow(ncfLastVisible))]).ToData)]);
    LAgent.Call('nyx_collections', 'Inspection test', LRequest);
    LApplied := Pair;
    LAgent.Call('nyx_collections', 'Inspection test', LRequest);
    Check(Pair = LApplied, 'an exact semantic operation retry is idempotent');
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LAgent.Revision, 'home').Design);
    try
      Check((LDocument.Find('reader-list').CollectionView.ColumnAt(0).Title = TNyxText('Reader 🌙')) and
        (LDocument.Find('reader-list').CollectionView.InspectionPolicy.MaximumPausedBytes = 1048576),
        'ordered partial inspection preserves preceding same-binding column edits');
    finally
      LDocument.Free;
    end;
    LRefused := False;
    try
      LAgent.Call('nyx_collections', 'Inspection test', NyxObject([
        NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('stale-inspection')),
        NyxField('changes', NyxCollectionPatch([NyxUseDefaultCollectionInspection(
          NyxBindingOwner('reader-list'), NyxCollection('readers'), cpList)]).ToData)]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pair = LApplied), 'stale semantic revision cannot reset accepted defaults');
    LAgent.Call('nyx_history', 'Inspection test', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('inspection-undo')), NyxField('direction', NyxData('undo'))]));
    Check(Pair = LBefore, 'one semantic Undo restores the whole source/design group');
    LAgent.Call('nyx_history', 'Inspection test', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('inspection-redo')), NyxField('direction', NyxData('redo'))]));
    Check(Pair = LApplied, 'one semantic Redo restores the whole group');
    LAgent.Call('nyx_collections', 'Inspection test', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('inspection-local')),
      NyxField('changes', NyxCollectionPatch([NyxSetCollectionInspection(
        NyxBindingOwner('first-records'), NyxCollection('readers'), cpList,
        NyxInspectionPolicy.PausedBytes(0))]).ToData)]));
    LReply := LAgent.Call('nyx_collections', 'Inspection test', NyxObject([
      NyxField('mode', NyxData('bindings')), NyxField('owner', NyxData('first-records')),
      NyxField('limit', NyxData(1))]));
    Check(LReply.Field('local').Field('inspection').Field('declared').AsBoolean and
      (LReply.Field('effective').Field('inspection').Field('policy').Field('maximumPausedBytes').AsInteger = 0),
      'inherited reusable part acquires independent explicit local defaults');
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LAgent.Revision, 'home').Design);
    try
      Check(LDocument.Find('definition-list').CollectionView.InspectionPolicy.MaximumPausedBytes = 32 * 1024 * 1024,
        'local authoring preserves the reusable definition and its peer defaults');
    finally
      LDocument.Free;
    end;
  finally
    LAgent.Free;
  end;
end;

begin
  try
    Inspector;
    Semantic;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-inspection-studio-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' inspection Studio checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-inspection-studio-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
