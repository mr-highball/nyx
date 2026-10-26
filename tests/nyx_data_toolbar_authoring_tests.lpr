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

program nyx_data_toolbar_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.composition, nyx.catalog, nyx.contract,
  nyx.data.toolbar, nyx.data.toolbar.editor, nyx.menu.editor,
  nyx.collections, nyx.collections.view.types, nyx.collections.query,
  nyx.studio.edits, nyx.studio.session
  {$ifdef PAS2JS}, Web{$endif};

const
  CCaption: TNyxText = 'Names 🌙';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Table(const AID: TNyxText): INyxTable;
begin
  Result := NewNyxTable(AID);
  Result.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('name'), 'Name').Scoped(csInstance));
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LToolbar: INyxDataToolbar;
  LWorkspace: INyxCard;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('people'),
      NyxCollectionSchema.Text(NyxTextField('name'), '').Integer(NyxIntegerField('rank'), 0), [
      NyxCollectionItem(NyxItem(NyxCollection('people'), 'ada')).WithValue(NyxTextField('name'), 'Ada')]);
    LPage := NewNyxPage('home');
    LToolbar := NewNyxDataToolbar('local-tools');
    LToolbar.CreateButton.WithText('Invite a person');
    LPage.Add(LToolbar);
    LPage.Add(Table('people'));
    LWorkspace := NewNyxCard('workspace');
    LToolbar := NewNyxDataToolbar('tools');
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LWorkspace.Add(LToolbar);
    LWorkspace.Add(Table('grid'));
    Result.AddComponent(LWorkspace);
    LInstance := NewNyxComponent('left');
    LInstance.Reference := NyxComponent('workspace');
    LInstance.Node.OverridePart(NyxPart('tools'), noAppend).Named('left-tools-rule').
      Add(NewNyxLabel('local-note').WithText('Keep this local note').Node);
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('right');
    LInstance.Reference := NyxComponent('workspace');
    LPage.Add(LInstance);
    LToolbar := NewNyxDataToolbar('toolbar-template');
    LToolbar.Add(Table('inner-table'));
    Result.AddComponent(LToolbar);
    LInstance := NewNyxComponent('whole');
    LInstance.Reference := NyxComponent('toolbar-template');
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function Options(const ATarget, ACaption: TNyxText): TNyxDataToolbarOptions;
begin
  Result := NyxDataToolbarOptions(NyxControl(ATarget), NyxTextField('name')).
    Sort(NyxToolbarSort(ACaption), NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending).
      ThenBy(NyxIntegerField('rank')));
end;

function FormRoot(ADocument: TNyxDocument; const AOwner: TNyxText): TNyxNode;
var
  LShell: TNyxDocument;
  LPage: INyxPage;
begin
  LShell := TNyxDocument.Create;
  try
    LPage := NewNyxPage('shell');
    LShell.AddPage(LPage);
    LPage.Add(NewNyxDataToolbarEditor('toolbar-editor', ADocument, NyxControl(AOwner)));
    Result := RealizeNyxView(LShell, LPage.Node);
  finally
    LShell.Free;
  end;
end;

procedure SetChoice(ARoot: TNyxNode; const APrefix: TNyxText;
  AField: TNyxDataToolbarEditorField; AIndex: Integer; const AValue: TNyxText);
begin
  ARoot.Find(NyxDataToolbarEditorFieldID(APrefix, AField)).SetProp(
    NyxAttributeName(atValue), NyxMenuEditorChoiceCaption(AIndex, AValue));
end;

procedure RoundTrip(ADocument: TNyxDocument);
var
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LSource: TNyxText;
begin
  LWorkspace := nil;
  LCandidate := nil;
  try
    LSource := TNyxCodegen.Generate(ADocument);
    LCandidate := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(ADocument),
      'typed emitted source preserves the complete candidate and selector provenance');
  finally
    LCandidate.Free;
    LWorkspace.Free;
  end;
end;

procedure SortContractCases;
var
  LToolbar: INyxDataToolbar;
  LReplacement: INyxDataToolbar;
  LOptions: TNyxDataToolbarOptions;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('copy-tools'), Options('people', 'First'));
  LBefore := TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON;
  LOptions := Options('people', 'Second').Baseline(NyxToolbarSort('Authored order'));
  LReplacement := ConfigureNyxDataToolbar(LToolbar, LOptions);
  Check((TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON = LBefore) and
    (LReplacement.SortSelect.Node.Prop(NyxAttributeName(atValue)) = 'Authored order'),
    'configured copy replaces its own choices and baseline without changing its input');
  SetNyxDataToolbarIntent(LReplacement, TNyxDataToolbarIntent.Disabled);
  LToolbar := ConfigureNyxDataToolbar(LReplacement, Options('people', 'Third'));
  Check(LToolbar.SortSelect.Node.Prop(NyxAttributeName(atValue)) = 'Original order',
    'saved selector provenance retains its base and default through a Disabled mask');
  LToolbar.SortSelect.Contract.Value(NyxTextDomain.Choices(['Fixed by caller']));
  LBefore := TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON;
  LRefused := False;
  try
    LReplacement := ConfigureNyxDataToolbar(LToolbar, Options('people', 'Fourth'));
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON = LBefore),
    'a later explicit caller contract cannot be weakened by saved toolbar provenance');
  LToolbar := NewNyxDataToolbar('restricted-tools');
  LToolbar.SortSelect.Contract.Value(NyxTextDomain.Choices(['Original order', 'Allowed']));
  LToolbar := ConfigureNyxDataToolbar(LToolbar, Options('people', 'Allowed'));
  LBefore := TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON;
  LRefused := False;
  try
    LReplacement := ConfigureNyxDataToolbar(LToolbar, Options('people', 'Not allowed'));
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (TNyxCodec.NodeSnapshot(LToolbar.Node).ToJSON = LBefore),
    'the original caller choice restriction survives a successful first configuration');
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LRoot: TNyxNode;
  LNext: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LChange: TNyxDataToolbarEditorChange;
  LRejected: TNyxDataToolbarEditorChange;
  LDraft: TNyxDataToolbarEditorDraft;
  LBefore: TNyxText;
  LSource: TNyxText;
  LAfter: TNyxText;
  LDefinition: TNyxText;
  LRow: TNyxText;
  LKey: TNyxText;
  LRefused: Boolean;
begin
  LDocument := Fixture;
  LSession := TNyxStudioSession.Create;
  LRoot := nil;
  LNext := nil;
  LContext := nil;
  LDraft.Clear;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LBefore := LSession.Save;
    LSource := LSession.Source;
    LRoot := FormRoot(LSession.Document, 'local-tools');
    LRow := NyxDataToolbarEditorSortID('toolbar-editor', 0);
    LKey := NyxDataToolbarEditorKeyID(LRow, 0);
    LRoot.Find(NyxDataToolbarEditorFieldID(LRow, ntefIncluded)).SetProp(NyxAttributeName(atValue), 'true');
    LRoot.Find(NyxDataToolbarEditorFieldID(LRow, ntefCaption)).SetProp(NyxAttributeName(atValue), CCaption);
    SetChoice(LRoot, LKey, ntefSortField, 1, 'name');
    Check(CaptureNyxDataToolbarEditor(LRoot.Find(NyxDataToolbarEditorActionID('toolbar-editor', nteaConfigure)),
      LRoot, LSession.Document, LChange), 'ordinary form captures typed target, search and sort');
    Check((LChange.Intent.Options.Target.ID = 'people') and
      (LChange.Intent.Options.ChoiceAt(0).Reference.Name = CCaption),
      'caption mapping retains exact typed references and supplementary Unicode');
    Check(TNyxDataToolbarEditorChange.FromData(LChange.ToData).ToData.ToJSON = LChange.ToData.ToJSON,
      'copied versioned change survives exact descriptor replay');
    SetChoice(LRoot, LKey, ntefSortField, 2, 'rank');
    SetChoice(LRoot, LKey, ntefSortComparison, 1, 'ASCII case insensitive');
    LRefused := False;
    try
      CaptureNyxDataToolbarEditor(LRoot.Find(NyxDataToolbarEditorActionID('toolbar-editor', nteaConfigure)),
        LRoot, LSession.Document, LRejected);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LBefore),
      'a text comparison cannot silently change or disappear on a numeric ordering field');
    SetChoice(LRoot, LKey, ntefSortField, 1, 'name');
    SetChoice(LRoot, LKey, ntefSortComparison, 0, 'Exact Unicode');
    LDraft.Capture('toolbar-editor', LRoot);
    LNext := FormRoot(LSession.Document, 'local-tools');
    Check(LDraft.Restore(LNext) and (LNext.Find(NyxDataToolbarEditorFieldID(LRow,
      ntefCaption)).Prop(NyxAttributeName(atValue)) = CCaption),
      'unsubmitted form drafts survive an unrelated shell replacement');
    Check(LSession.Save = LBefore, 'building and capturing the form changes no accepted design');
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LAfter := LSession.Save;
    Check((LAfter <> LBefore) and (LSession.Source <> LSource),
      'one ordinary Studio patch publishes toolbar roles, metadata and crafted source');
    Check(LSession.Document.Find('local-tools').Part(NyxPart('clear')).ProjectionKind =
      NyxKindName(nkButton), 'the complete candidate contains an accessible ordinary Clear button');
    Check(LSession.Document.Find('local-tools').Part(NyxPart('create')).Prop(NyxAttributeName(atText)) =
      'Invite a person', 'configuring the toolbar preserves its customized existing parts');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LSource),
      'one Undo restores the entire exact design and source pair');
    LSession.Redo;
    Check(LSession.Save = LAfter, 'one Redo reinstates the complete configuration');
    RoundTrip(LSession.Document);
    LRefused := False;
    try
      LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LAfter), 'a stale mounted proposal cannot rebase silently');
    LDefinition := TNyxCodec.NodeSnapshot(LSession.Document.Find('workspace')).ToJSON;
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('left-tools-rule'), Options('grid', CCaption));
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LContext := RealizeNyxContext(LSession.Document, LSession.Document.Find('left-tools-rule'), LProjection);
    Check(GetNyxDataToolbarIntent(LProjection).Options.Count = 1, 'named instance intent configures its own toolbar');
    Check(LProjection.Part(NyxPart('clear')).ProjectionKind = NyxKindName(nkButton),
      'named append descriptor acquires Clear with its original payload intact');
    Check(LSession.Document.Find('local-note').Prop(NyxAttributeName(atText)) = 'Keep this local note',
      'named append payload is retained');
    FreeAndNil(LContext);
    Check(TNyxCodec.NodeSnapshot(LSession.Document.Find('workspace')).ToJSON = LDefinition,
      'instance editing never mutates the shared definition');
    LContext := RealizeNyxContext(LSession.Document, LSession.Document.Find('right'), LProjection);
    Check(not HasNyxDataToolbarIntent(LProjection.Part(NyxPart('tools'))),
      'the other reusable occurrence retains static behavior');
    FreeAndNil(LContext);
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('left-tools-rule'), nteaDisable);
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('left-tools-rule'), Options('grid', 'Rank and name'));
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    Check(GetNyxDataToolbarIntent(LSession.Document.Find('left-tools-rule')).Options.
      ChoiceAt(0).Reference.Name = 'Rank and name', 'Disable and reconfigure can replace toolbar-owned choices');
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('left-tools-rule'), nteaInherit);
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    Check(not HasNyxDataToolbarIntent(LSession.Document.Find('left-tools-rule')),
      'Inherit removes the local mask without deleting authored payload');
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('toolbar-template'), Options('inner-table', 'Definition order'));
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('whole'), Options('inner-table', 'Local order'));
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('whole'), nteaInherit);
    LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    LContext := RealizeNyxContext(LSession.Document, LSession.Document.Find('whole'), LProjection);
    Check(GetNyxDataToolbarIntent(LProjection).Options.ChoiceAt(0).Reference.Name = 'Definition order',
      'whole-reference inheritance reconciles the previous selector to its inherited choices');
    FreeAndNil(LContext);
    RoundTrip(LSession.Document);
    LRefused := False;
    LBefore := LSession.Save;
    try
      LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('left-tools-rule'), Options('people', 'Foreign'));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LBefore), 'foreign global target refuses within a reusable workspace');
    LChange := NyxDataToolbarEditorChange(LSession.Document, NyxControl('local-tools'), nteaDisable);
    LSession.SetSourceDraft('unfinished Pascal');
    LRefused := False;
    try
      LSession.ApplyPatch(NyxDataToolbarPolicyPatch(LChange));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LBefore) and (LSession.DraftSource = 'unfinished Pascal'),
      'pending handwritten source refuses without losing either accepted design or draft');
  finally
    LContext.Free;
    LNext.Free;
    LRoot.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

begin
  try
    SortContractCases;
    Run;
    {$ifdef PAS2JS}
    Web.document.body.setAttribute('data-toolbar-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' DataToolbar authoring checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-toolbar-authoring-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
