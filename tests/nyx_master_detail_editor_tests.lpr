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

program nyx_master_detail_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.composition, nyx.collections, nyx.collections.registry,
  nyx.collections.view.types, nyx.collections.selection, nyx.master.detail,
  nyx.master.detail.editor
  {$ifdef PAS2JS}, Web{$endif};

type
  { Real authored roots exercise direct, whole-reference and named-descriptor
    ownership. Collections and tree snapshots are ordinary product values. }
  TEditorFixture = class
    Document: TNyxDocument;
    Local: INyxMasterDetail;
    StaticDetail: INyxMasterDetail;
    Definition: INyxMasterDetail;
    First: INyxComponent;
    Second: INyxComponent;
    Named: INyxComponent;
    Rule: TNyxNode;
    Options: TNyxMasterDetailOptions;
    constructor Create;
    destructor Destroy; override;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create('MasterDetail editor: ' + String(AReason));
  end;
end;

function PeopleSchema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('short-name'), '').
    Text(NyxTextField('full-name'), '').Text(NyxTextField('summary'), '').
    Integer(NyxIntegerField('score'), 0);
end;

procedure BindMaster(const ADetail: INyxMasterDetail);
begin
  ADetail.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
    Column(NyxTextField('short-name'), 'People').Scoped(csInstance)).Done;
end;

constructor TEditorFixture.Create;
var
  LPage: INyxPage;
  LContainer: INyxColumn;
  LNested: INyxMasterDetail;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  Options := NyxMasterDetailOptions(NyxTextField('full-name'), NyxTextField('summary'));
  Document.Collections.Define(NyxCollection('people'), PeopleSchema, [
    NyxCollectionItem(NyxItem(NyxCollection('people'), 'one')).
      WithValue(NyxTextField('short-name'), 'Ada').
      WithValue(NyxTextField('full-name'), 'Ada Lovelace').
      WithValue(NyxTextField('summary'), 'First record')]);
  LPage := NewNyxPage('home');
  Document.AddPage(LPage);
  Local := NewNyxMasterDetail('local-detail');
  BindMaster(Local);
  LPage.Add(Local);
  StaticDetail := NewNyxMasterDetail('static-detail');
  LPage.Add(StaticDetail);
  Definition := NewNyxMasterDetail('detail-definition');
  BindMaster(Definition);
  SetNyxMasterDetailIntent(Definition, TNyxMasterDetailIntent.Configured(Options));
  Document.AddComponent(Definition);
  First := NewNyxComponent('first-detail');
  First.Reference := NyxComponent('detail-definition');
  LPage.Add(First);
  Second := NewNyxComponent('second-detail');
  Second.Reference := NyxComponent('detail-definition');
  LPage.Add(Second);
  LContainer := NewNyxColumn('container-definition');
  LNested := NewNyxMasterDetail('nested-detail');
  LNested.Configure.PartName(NyxPart('directory')).Done;
  BindMaster(LNested);
  SetNyxMasterDetailIntent(LNested, TNyxMasterDetailIntent.Configured(Options));
  LContainer.Add(LNested);
  Document.AddComponent(LContainer);
  Named := NewNyxComponent('named-detail');
  Named.Reference := NyxComponent('container-definition');
  Rule := Named.Node.OverridePart(NyxPart('directory')).Named('directory-rule');
  LPage.Add(Named);
end;

destructor TEditorFixture.Destroy;
begin
  Document.Free;
  inherited Destroy;
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
    LPage.Add(NewNyxMasterDetailEditor('detail-editor', ADocument, NyxControl(AOwner)));
    Result := RealizeNyxView(LShell, LPage.Node);
  finally
    LShell.Free;
  end;
end;

function Action(ARoot: TNyxNode; AAction: TNyxMasterDetailEditorAction): TNyxNode;
begin
  Result := ARoot.Find(NyxMasterDetailEditorActionID('detail-editor', AAction));
end;

function Input(ARoot: TNyxNode; ADescription: Boolean): TNyxNode;
begin
  Result := ARoot.Find(NyxMasterDetailEditorFieldID('detail-editor', ADescription));
end;

procedure SetFields(ARoot: TNyxNode; const ATitle, ADescription: TNyxText);
begin
  Input(ARoot, False).SetProp(NyxAttributeName(atValue), ATitle);
  Input(ARoot, True).SetProp(NyxAttributeName(atValue), ADescription);
end;

function CaptureRefused(ARoot: TNyxNode; ADocument: TNyxDocument;
  AAction: TNyxMasterDetailEditorAction): Boolean;
var
  LChange: TNyxMasterDetailEditorChange;
begin
  Result := False;
  try
    CaptureNyxMasterDetailEditor(Action(ARoot, AAction), ARoot, ADocument, LChange);
  except
    on Exception do
    begin
      Result := True;
    end;
  end;
end;

function ChangeRefused(ADocument: TNyxDocument;
  const AChange: TNyxMasterDetailEditorChange): Boolean;
begin
  Result := False;
  try
    ValidateNyxMasterDetailEditorChange(ADocument, AChange);
  except
    on Exception do
    begin
      Result := True;
    end;
  end;
end;

procedure FormAndCopyCases;
var
  LFixture: TEditorFixture;
  LRoot: TNyxNode;
  LOther: TNyxNode;
  LChange: TNyxMasterDetailEditorChange;
  LCopy: TNyxMasterDetailEditorChange;
  LRead: TNyxMasterDetailEditorChange;
  LBefore: TNyxText;
  LSpec: TNyxText;
  LRefused: Boolean;
begin
  LFixture := TEditorFixture.Create;
  LRoot := nil;
  LOther := nil;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LSpec := LFixture.Local.MasterList.Node.CollectionView.ToData.ToJSON;
    LRoot := FormRoot(LFixture.Document, 'local-detail');
    Check(Action(LRoot, ndmaConfigure).Prop(NyxAttributeName(atEnabled), 'true') = 'true',
      'a real bound SINGLE list admits Configure without changing its display column');
    SetFields(LRoot, 'full-name', 'summary');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaConfigure), LRoot,
      LFixture.Document, LChange), 'both mounted editable inputs capture a complete proposal');
    Check((LChange.Owner.ID = 'local-detail') and (LChange.Action = ndmaConfigure) and
      not LChange.Inherit and (LChange.Intent.Options.TitleField.Name = 'full-name') and
      (LChange.Intent.Options.DescriptionField.Name = 'summary'),
      'copied intent carries only the exact typed mapped fields and authored owner');
    LCopy := LChange.Copy;
    LRead := TNyxMasterDetailEditorChange.FromData(LChange.ToData);
    Check(LRead.ToData.ToJSON = LChange.ToData.ToJSON, 'configured wire round trip is exact');
    ValidateNyxMasterDetailEditorChange(LFixture.Document, LRead);
    SetFields(LRoot, 'summary', 'full-name');
    Check(LCopy.Intent.Options.TitleField.Name = 'full-name',
      'later input drafts cannot alter an earlier copied change');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'construction, input drafts, capture and validation mutate no project/defaults');
    Check(LFixture.Local.MasterList.Node.CollectionView.ToData.ToJSON = LSpec,
      'source, scope, query and authored columns remain owned by the master list');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaInherit),
      'absence is not a removable local policy');
    Check(not CaptureNyxMasterDetailEditor(Input(LRoot, False), LRoot,
      LFixture.Document, LRead), 'an input notification cannot masquerade as an action');
    LOther := FormRoot(LFixture.Document, 'local-detail');
    LRefused := False;
    try
      CaptureNyxMasterDetailEditor(Action(LRoot, ndmaConfigure), LOther,
        LFixture.Document, LRead);
    except
      on ENyxMasterDetail do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'same-ID foreign mounted form has no capture authority');
    LFixture.Local.DetailTitleHeading.WithText('Changed placeholder');
    Check(ChangeRefused(LFixture.Document, LCopy) and
      CaptureRefused(LRoot, LFixture.Document, ndmaConfigure),
      'exact authored/effective role changes stale both held proposals and mounted forms');
  finally
    LOther.Free;
    LRoot.Free;
    LFixture.Free;
  end;
  Check((LCopy.Owner.ID = 'local-detail') and
    (LCopy.Intent.Options.DescriptionField.Name = 'summary') and (LCopy.Baseline <> ''),
    'detached proposal remains readable after document and forms retire');
end;

procedure DraftAndSchemaCases;
var
  LFixture: TEditorFixture;
  LRoot: TNyxNode;
  LNext: TNyxNode;
  LDraft: TNyxMasterDetailEditorDraft;
  LCopy: TNyxMasterDetailEditorDraft;
  LChange: TNyxMasterDetailEditorChange;
  LRead: TNyxMasterDetailEditorChange;
  LBefore: TNyxText;
  LBaseline: TNyxText;
begin
  LFixture := TEditorFixture.Create;
  LRoot := nil;
  LNext := nil;
  LDraft.Clear;
  LCopy.Clear;
  try
    LRoot := FormRoot(LFixture.Document, 'local-detail');
    LBefore := TNyxCodec.Encode(LFixture.Document);
    SetFields(LRoot, 'full-name', 'summary');
    CaptureNyxMasterDetailEditor(Action(LRoot, ndmaConfigure), LRoot,
      LFixture.Document, LChange);
    SetFields(LRoot, 'missing', 'summary');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaConfigure),
      'an unknown title draft refuses before a proposal is returned');
    SetFields(LRoot, 'full-name', 'score');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaConfigure),
      'the second draft independently requires the exact text field family');
    SetFields(LRoot, '', 'summary');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaConfigure),
      'empty typed field names never choose a fallback at capture');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaDisable), LRoot,
      LFixture.Document, LRead) and (LRead.Intent.Mode = nmiDisabled),
      'Disable remains useful while unrelated Configure drafts are invalid');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'invalid drafts and a copied disabling proposal mutate no authored state');
    SetFields(LRoot, 'unsubmitted-title', 'unsubmitted-description');
    LDraft.Capture('detail-editor', LRoot);
    LCopy := LDraft;
    LDraft.Clear;
    LNext := FormRoot(LFixture.Document, 'local-detail');
    Check(LCopy.Restore(LNext) and
      (Input(LNext, False).Prop(NyxAttributeName(atValue)) = 'unsubmitted-title') and
      (Input(LNext, True).Prop(NyxAttributeName(atValue)) = 'unsubmitted-description'),
      'copied draft parks both scalar strings independently of the original record');
    Check(not LCopy.Restore(nil), 'an absent panel parks rather than retires the draft');
    LBaseline := NyxMasterDetailEditorBaseline(LFixture.Document, NyxControl('local-detail'));
    LFixture.Document.Collections.Define(NyxCollection('people'), PeopleSchema, [
      NyxCollectionItem(NyxItem(NyxCollection('people'), 'replacement')).
        WithValue(NyxTextField('short-name'), 'Another row')]);
    Check(LBaseline = NyxMasterDetailEditorBaseline(LFixture.Document,
      NyxControl('local-detail')), 'row contents and revisions are not configuration authority');
    ValidateNyxMasterDetailEditorChange(LFixture.Document, LChange);
    Check(not ChangeRefused(LFixture.Document, LChange),
      'row-only replacement keeps the declared source/schema proposal current');
    LFixture.Document.Collections.Define(NyxCollection('people'),
      PeopleSchema.Text(NyxTextField('extra'), ''), []);
    Check(ChangeRefused(LFixture.Document, LChange),
      'a changed collection declaration stales the exact source-context baseline');
    FreeAndNil(LNext);
    LNext := FormRoot(LFixture.Document, 'local-detail');
    Check(not LCopy.Restore(LNext), 'changed source declaration retires parked input drafts');
    LFixture.Document.Collections.Define(NyxCollection('people'), PeopleSchema, []);
    FreeAndNil(LNext);
    LNext := FormRoot(LFixture.Document, 'local-detail');
    Check(not LCopy.Restore(LNext), 'a retired draft cannot revive after the old schema returns');
  finally
    LNext.Free;
    LRoot.Free;
    LFixture.Free;
  end;
end;

procedure TargetAndInheritanceCases;
var
  LFixture: TEditorFixture;
  LRoot: TNyxNode;
  LChange: TNyxMasterDetailEditorChange;
  LRead: TNyxMasterDetailEditorChange;
  LBefore: TNyxText;
  LDefinition: TNyxText;
  LSecond: TNyxText;
  LRefused: Boolean;
  LIntent: TNyxMasterDetailIntent;
begin
  LFixture := TEditorFixture.Create;
  LRoot := nil;
  try
    LDefinition := TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON;
    LSecond := TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON;
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LRoot := FormRoot(LFixture.Document, 'first-detail');
    Check((Input(LRoot, False).Prop(NyxAttributeName(atValue)) = 'full-name') and
      (Input(LRoot, True).Prop(NyxAttributeName(atValue)) = 'summary'),
      'whole reusable selection displays inherited field mappings');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaConfigure), LRoot,
      LFixture.Document, LChange) and (LChange.Owner.ID = 'first-detail'),
      'a local configuration proposal retains the whole authored reference owner');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaInherit),
      'inherited intent alone cannot be removed from the definition');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaDisable), LRoot,
      LFixture.Document, LRead) and (LRead.Intent.Mode = nmiDisabled),
      'explicit Disable can mask an inherited configuration');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'copied whole-reference proposals mutate neither source nor defaults');
    SetNyxMasterDetailIntent(LFixture.First, TNyxMasterDetailIntent.Disabled);
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, 'first-detail');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaInherit), LRoot,
      LFixture.Document, LRead) and LRead.Inherit,
      'a local mask offers an explicit inheritance proposal');
    Check(LRead.ToData.Field('intent').Kind = ndNull,
      'inherit wire uses null without serializing undefined intent');
    LChange := TNyxMasterDetailEditorChange.FromData(LRead.ToData);
    Check(LChange.Inherit and (LChange.Action = ndmaInherit),
      'inherit action survives a strict copied round trip');
    LRefused := False;
    try
      LIntent := LRead.Intent;
      LIntent.Validate;
    except
      on ENyxMasterDetail do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'inherit getter never fabricates a disabled or configured local value');
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, 'directory-rule');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaConfigure), LRoot,
      LFixture.Document, LRead) and (LRead.Owner.ID = 'directory-rule'),
      'named-part capture preserves the exact descriptor rather than a projected ID');
    Check((TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON = LSecond),
      'whole and named form work preserves definition and sibling exactly');
    LFixture.Rule.SetProp(NyxAttributeName(atOverrideMode), NyxOverrideName(noRemove));
    Check(ChangeRefused(LFixture.Document, LRead),
      'a removed named part refuses its earlier configuration proposal');
  finally
    LRoot.Free;
    LFixture.Free;
  end;
end;

procedure InertAndShapeCases;
var
  LFixture: TEditorFixture;
  LRoot: TNyxNode;
  LChange: TNyxMasterDetailEditorChange;
  LBefore: TNyxText;
  LIndex: Integer;
  LRefused: Boolean;
  LForm: INyxCard;
begin
  LFixture := TEditorFixture.Create;
  LRoot := nil;
  LForm := nil;
  try
    LRoot := FormRoot(LFixture.Document, 'static-detail');
    Check((Action(LRoot, ndmaConfigure).Prop(NyxAttributeName(atEnabled)) = 'false') and
      (LRoot.Find('detail-editor-status').Prop(NyxAttributeName(atText)) <> ''),
      'an unbound static detail displays helpful inert Configure rather than raising');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaConfigure),
      'disabled Configure cannot capture an unbound source');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaDisable), LRoot,
      LFixture.Document, LChange) and (LChange.Intent.Mode = nmiDisabled),
      'static details may still opt out explicitly');
    SetNyxMasterDetailIntent(LFixture.StaticDetail, TNyxMasterDetailIntent.Disabled);
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, 'static-detail');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaInherit), LRoot,
      LFixture.Document, LChange) and LChange.Inherit,
      'static local masks may still inherit without needing a collection');
    FreeAndNil(LRoot);
    LFixture.Local.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('short-name'), 'People').Selection(nsmMultiple)).Done;
    LRoot := FormRoot(LFixture.Document, 'local-detail');
    Check(CaptureRefused(LRoot, LFixture.Document, ndmaConfigure) and
      CaptureNyxMasterDetailEditor(Action(LRoot, ndmaDisable), LRoot,
        LFixture.Document, LChange), 'multiple selection is incompatible but Disable stays useful');
    FreeAndNil(LRoot);
    BindMaster(LFixture.Local);
    LFixture.Document.Collections.Remove(NyxCollection('people'));
    LRefused := False;
    try
      LRoot := FormRoot(LFixture.Document, 'local-detail');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRoot = nil),
      'an invalid missing collection declaration retains complete model admission refusal');
    LFixture.Document.Collections.Define(NyxCollection('people'), PeopleSchema, []);
    LBefore := TNyxCodec.Encode(LFixture.Document);
    for LIndex := 0 to 3 do
    begin
      LRoot := FormRoot(LFixture.Document, 'local-detail');
      case LIndex of
        0:
          begin
            Input(LRoot, True).Configure.ProjectAs(nkLabel).Done;
          end;
        1:
          begin
            Input(LRoot, False).SetProp('nyx.master-detail-editor.field', 'other');
          end;
        2:
          begin
            Action(LRoot, ndmaInherit).SetProp('nyx.master-detail-editor.action', 'Disable');
          end;
        3:
          begin
            LRoot.Find('detail-editor').SetProp('nyx.master-detail-editor.version', '2');
          end;
      end;
      Check(CaptureRefused(LRoot, LFixture.Document, ndmaDisable),
        'a malformed required sibling refuses the whole form case ' + TNyxText(IntToStr(LIndex)));
      FreeAndNil(LRoot);
    end;
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'malformed mounted content mutates no document');
    LRefused := False;
    try
      LForm := NewNyxMasterDetailEditor('wrong-editor', LFixture.Document, NyxControl('home'));
    except
      on ENyxMasterDetail do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LForm = nil), 'a foreign target family refuses before publishing a form');
  finally
    LRoot.Free;
    LFixture.Free;
  end;
end;

procedure EmptyCollectionsCases;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LDetail: INyxMasterDetail;
  LRoot: TNyxNode;
  LChange: TNyxMasterDetailEditorChange;
  LBefore: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  try
    LPage := NewNyxPage('empty-home');
    LDocument.AddPage(LPage);
    LDetail := NewNyxMasterDetail('empty-detail');
    LPage.Add(LDetail);
    LBefore := TNyxCodec.Encode(LDocument);
    LRoot := FormRoot(LDocument, 'empty-detail');
    Check((LDocument.Collections.Count = 0) and
      CaptureRefused(LRoot, LDocument, ndmaConfigure),
      'a valid empty collection catalog presents an inert Configure action');
    Check(CaptureNyxMasterDetailEditor(Action(LRoot, ndmaDisable), LRoot,
      LDocument, LChange) and (LChange.Intent.Mode = nmiDisabled),
      'explicit disabling needs no fabricated collection or placeholder schema');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'empty-catalog editor construction and capture create no project defaults');
  finally
    LRoot.Free;
    LDocument.Free;
  end;
end;

procedure WireCases;
const
  CInvalid: array[0..9] of TNyxText = (
    'null', '{}',
    '{"version":2,"owner":"x","action":"inherit","baseline":"b","intent":null}',
    '{"version":1,"owner":"","action":"inherit","baseline":"b","intent":null}',
    '{"version":1,"owner":"x","action":"Inherit","baseline":"b","intent":null}',
    '{"version":1,"owner":"x","action":"inherit","baseline":"","intent":null}',
    '{"version":1,"owner":"x","action":"configure","baseline":"b","intent":null}',
    '{"version":1,"owner":"x","action":"inherit","baseline":"b",' +
      '"intent":{"version":1,"mode":"disabled"}}',
    '{"version":1,"owner":"x","action":"disable","baseline":"b",' +
      '"intent":{"version":1,"mode":"configured",' +
      '"options":{"version":1,"title":"a","description":"b"}}}',
    '{"version":1,"owner":"x","action":"inherit","baseline":"b",' +
      '"intent":null,"extra":0}');
var
  LIndex: Integer;
  LRead: TNyxMasterDetailEditorChange;
  LRefused: Boolean;
  LFixture: TEditorFixture;
begin
  for LIndex := Low(CInvalid) to High(CInvalid) do
  begin
    LRefused := False;
    try
      LRead := TNyxMasterDetailEditorChange.FromData(TNyxDataValue.ParseJSON(CInvalid[LIndex]));
      LRead.ToData;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'strict change wire refuses malformed case ' + TNyxText(IntToStr(LIndex)));
  end;
  LFixture := TEditorFixture.Create;
  try
    LRefused := False;
    try
      LRead := NyxMasterDetailEditorChange(LFixture.Document,
        NyxControl('local-detail'), ndmaConfigure);
    except
      on ENyxMasterDetail do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Configure never manufactures missing typed options');
    LRead := NyxMasterDetailEditorChange(LFixture.Document,
      NyxControl('local-detail'), ndmaDisable);
    Check(TNyxMasterDetailEditorChange.FromData(LRead.ToData).Intent.Mode = nmiDisabled,
      'explicit disabled wire retains its independent local mask');
  finally
    LFixture.Free;
  end;
end;

begin
  try
    FormAndCopyCases;
    DraftAndSchemaCases;
    TargetAndInheritanceCases;
    InertAndShapeCases;
    EmptyCollectionsCases;
    WireCases;
    WriteLn('PASS ', GChecks, ' MasterDetail editor checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-master-detail-editor', 'passed');
    document.body.setAttribute('data-master-detail-editor-checks', IntToStr(GChecks));
    {$endif}
  except
    on E: Exception do
    begin
      WriteLn('FAIL at check ', GChecks, ': ', E.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-master-detail-editor', 'failed');
      document.body.setAttribute('data-master-detail-editor-error', E.Message);
      {$else}
      Halt(1);
      {$endif}
    end;
  end;
end.
