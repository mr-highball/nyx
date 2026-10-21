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
program nyx_parts_editor_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}Web,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.catalog,
  nyx.codec, nyx.codegen, nyx.composition, nyx.parts.editor, nyx.studio.edits;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
    raise Exception.Create(AReason);
end;

{ Ordinary authored controls share one reusable definition. Their event names
  are inert metadata; this consumer never dispatches or invokes application code. }
function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LDefinition: INyxColumn;
  LActions: INyxRow;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    Result.AddPage(LPage);
    LDefinition := NewNyxColumn('card-definition');
    LDefinition.Configure.Compound(True).Done;
    LDefinition.Add(NewNyxLabel('card-title').Configure.PartName(NyxPart('title'))
      .Text('A reusable card').Done);
    LActions := NewNyxRow('card-actions');
    LActions.Configure.PartName(NyxPart('actions')).Done;
    LActions.Add(NewNyxButton('card-send').Configure.PartName(NyxPart('send'))
      .Text('Send').OnClick(NyxEvent('application-send')).Done);
    LDefinition.Add(LActions);
    LDefinition.Add(NewNyxLabel('card-unicode').Configure.PartName(NyxPart('标题🌙<&>'))
      .Text('Literal part text').Done);
    Result.AddComponent(LDefinition);
    LPage.Add(NewNyxComponent('first').Configure.Component(NyxComponent('card-definition')).Done);
    LPage.Add(NewNyxComponent('second').Configure.Component(NyxComponent('card-definition')).Done);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function RowAction(const AForm: INyxCard; const APath: TNyxText): TNyxNode;
var
  LBody, LRow: TNyxNode;
  LAt: Integer;
begin
  Result := nil;
  LBody := AForm.Part(NyxPart('rows')).Node;
  for LAt := 0 to LBody.Count - 1 do
  begin
    LRow := LBody.Children[LAt];

    if LRow.ProjectionKind <> NyxKindName(nkColumn) then
      Continue;

    if LRow.Part(NyxPart('path')).Prop(NyxAttributeName(atText)) = APath then
      Exit(LRow.Part(NyxPart('action')));
  end;
  raise Exception.Create('The complete Parts form is missing ' + APath);
end;

function HasPart(ADocument: TNyxDocument; const AInstance, APath: TNyxText): Boolean;
var
  LRuntime: TNyxNode;
begin
  Result := False;
  LRuntime := RealizeNyxView(ADocument, ADocument.Find(AInstance));
  try
    try
      Result := LRuntime.Part(NyxPart(APath)) <> nil;
    except
      on ENyxModel do Result := False;
    end;
  finally
    LRuntime.Free;
  end;
end;

{ The product's existing typed patch creates the candidate; this is not a
  duplicate implementation of hide/remove/inherit semantics. Root's ordinary
  Studio consumer separately owns paired source/history and actual input. }
function Candidate(ADocument: TNyxDocument;
  const AProposal: TNyxPartsEditorProposal): TNyxDocument;
var
  LCatalog: TNyxCatalog;
  LChange: TNyxReusableChange;
  LPatch: INyxDesignPatch;
begin
  ValidateNyxPartsEditorProposal(ADocument, AProposal);

  if AProposal.Action = npaHide then
    LChange := NyxOverrideComponentPart(AProposal.Instance, AProposal.Descriptor,
      AProposal.Path, noRemove)
  else
    LChange := NyxInheritComponentPart(AProposal.Instance, AProposal.Descriptor, AProposal.Path);
  LPatch := NyxReusablePatch([LChange]);
  LCatalog := TNyxCatalog.Create;
  try
    Result := LPatch.Candidate(ADocument, LCatalog);
  finally
    LCatalog.Free;
  end;
end;

procedure HideRestore;
var
  LDocument, LHidden, LRestored, LDecoded, LNested: TNyxDocument;
  LForm, LSecondForm, LHiddenForm: INyxCard;
  LProposal, LPrior, LRestore: TNyxPartsEditorProposal;
  LBefore, LDefinition, LOther, LCode: TNyxText;
  LButton: TNyxNode;
begin
  LDocument := Fixture;
  LHidden := nil;
  LRestored := nil;
  LDecoded := nil;
  LNested := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LDefinition := TNyxCodec.NodeSnapshot(LDocument.Find('card-definition')).ToJSON;
    LOther := TNyxCodec.NodeSnapshot(LDocument.Find('second')).ToJSON;
    LForm := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    LSecondForm := NewNyxPartsEditor('other-parts', LDocument, NyxControl('second'));
    Check((LForm.Part(NyxPart('rows')).Count = 4) and
      (LSecondForm.Part(NyxPart('rows')).Count = 4), 'Both public forms list all named and nested parts');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'Constructing two forms changes no document data');
    LButton := RowAction(LForm, 'actions');
    Check(CaptureNyxPartsEditor(LButton, LForm.Node, LDocument, LProposal),
      'A live owned Parts action captures one typed proposal');
    Check((LProposal.Instance.ID = 'first') and (LProposal.Path.Name = 'actions') and
      (LProposal.Action = npaHide) and (LProposal.Descriptor.ID <> '') and
      (LProposal.Baseline = NyxPartsEditorBaseline(LDocument, NyxControl('first'))),
      'Proposal owns exact instance/path/action/descriptor and saved baseline');
    LPrior := LProposal;
    LHidden := Candidate(LDocument, LProposal);
    Check(not HasPart(LHidden, 'first', 'actions') and HasPart(LHidden, 'second', 'actions/send'),
      'Only the selected instance loses the named subtree');
    Check((TNyxCodec.NodeSnapshot(LHidden.Find('card-definition')).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LHidden.Find('second')).ToJSON = LOther) and
      (TNyxCodec.Encode(LDocument) = LBefore), 'Definition, sibling instance and source document remain exact');
    LHiddenForm := NewNyxPartsEditor('parts', LHidden, NyxControl('first'));
    Check(LHiddenForm.Part(NyxPart('rows')).Count = 3,
      'The hidden parent remains a row while its removed descendants are not fabricated');
    Check(CaptureNyxPartsEditor(RowAction(LHiddenForm, 'actions'), LHiddenForm.Node,
      LHidden, LRestore) and (LRestore.Action = npaRestore) and
      (LRestore.Descriptor.ID = LPrior.Descriptor.ID), 'Hidden rows retain the exact Restore descriptor');
    Check(Pos('inherits the definition', LHiddenForm.Node.Find('parts-help').Prop(NyxAttributeName(atText))) > 0,
      'Restore original explains inheritance rather than a cache of former customizations');
    LRestored := Candidate(LHidden, LRestore);
    Check(HasPart(LRestored, 'first', 'actions/send') and
      (LRestored.Find(LRestore.Descriptor.ID) = nil), 'Restore removes the local descriptor and inherits original content');
    Check(TNyxCodec.Encode(LRestored) = LBefore, 'Hide then Restore returns the complete original document');
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LHidden));
    Check(not HasPart(LDecoded, 'first', 'actions') and HasPart(LDecoded, 'second', 'actions'),
      'Ordinary saved data retains independent hidden-part intent');
    LCode := TNyxCodegen.Generate(LHidden);
    Check((Pos('noRemove', LCode) > 0) and (Pos('actions', LCode) > 0),
      'The normal Pascal generator retains typed remove intent');
    LProposal := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('actions/send'), npaHide);
    LNested := Candidate(LDocument, LProposal);
    Check(HasPart(LNested, 'first', 'actions') and not HasPart(LNested, 'first', 'actions/send') and
      HasPart(LNested, 'second', 'actions/send'), 'Nested removal preserves its parent and independent instance');
    LProposal := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('标题🌙<&>'), npaHide);
    Check((LProposal.Path.Name = TNyxText('标题🌙<&>')) and
      (Pos(TNyxText('标题'), RowAction(LForm, '标题🌙<&>').ID) = 0), 'Unicode/markup part names stay inert data outside chrome IDs');
    Check((LPrior.Path.Name = 'actions') and (LPrior.Action = npaHide),
      'Later proposals leave the earlier copied intent unchanged');
  finally
    LNested.Free;
    LDecoded.Free;
    LRestored.Free;
    LHidden.Free;
    LDocument.Free;
  end;
end;

procedure Refusals;
var
  LDocument, LCollision: TNyxDocument;
  LForm, LForeign: INyxCard;
  LButton, LRule: TNyxNode;
  LProposal, LPrior: TNyxPartsEditorProposal;
  LBefore, LPriorBaseline: TNyxText;
  LFailed: Boolean;

  procedure Refuse(const APath: TNyxText; AAction: TNyxPartsEditorAction; const AReason: TNyxText);
  var
    LRejected: Boolean;
    LCurrent: TNyxText;
  begin
    LCurrent := TNyxCodec.Encode(LDocument);
    LRejected := False;
    try
      NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart(APath), AAction);
    except
      on ENyxModel do LRejected := True;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LCurrent), AReason);
  end;

begin
  LDocument := Fixture;
  LCollision := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LForm := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    LPrior := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('actions'), npaHide);
    LPriorBaseline := LPrior.Baseline;
    Refuse('.', npaHide, 'The reusable root cannot be hidden');
    Refuse('', npaHide, 'An absent part cannot become a root action');
    Refuse('missing', npaHide, 'Unknown named paths refuse without editing');
    Refuse('actions', npaRestore, 'Restoring an inherited visible part cannot remove another descriptor');
    Check(not CaptureNyxPartsEditor(nil, nil, nil, LProposal), 'Unrelated input leaves capture unhandled');
    LForeign := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    LFailed := False;
    try
      CaptureNyxPartsEditor(RowAction(LForeign, 'actions'), LForm.Node, LDocument, LProposal);
    except
      on ENyxModel do LFailed := True;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore), 'A same-ID button from another form cannot authorize an edit');
    LButton := RowAction(LForm, 'actions');
    LButton.SetProp('nyx.parts-editor.row', '999999');
    LFailed := False;
    try CaptureNyxPartsEditor(LButton, LForm.Node, LDocument, LProposal);
    except on ENyxModel do LFailed := True; end;
    Check(LFailed, 'Forged row ordinals refuse rather than redirecting a part action');
    LForm := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    LDocument.Find('card-title').SetProp(NyxAttributeName(atText), 'Changed definition');
    LFailed := False;
    try ValidateNyxPartsEditorProposal(LDocument, LPrior);
    except on ENyxModel do LFailed := True; end;
    Check(LFailed, 'Same-ID definition content changes retire prior proposals');
    LFailed := False;
    try CaptureNyxPartsEditor(RowAction(LForm, 'actions'), LForm.Node, LDocument, LProposal);
    except on ENyxModel do LFailed := True; end;
    Check(LFailed, 'A stale mounted baseline refuses before an action can be queued');
    LDocument.Find('card-title').SetProp(NyxAttributeName(atText), 'A reusable card');
    LCollision := LDocument.Clone;
    LCollision.Pages[0].Add(NewNyxLabel(LPrior.Descriptor.ID));
    LFailed := False;
    try ValidateNyxPartsEditorProposal(LCollision, LPrior);
    except on ENyxModel do LFailed := True; end;
    Check(LFailed, 'A newly occupied descriptor identity retires the exact earlier proposal');
    LRule := LDocument.Find('first').OverridePart(NyxPart('actions'), noAppend).Named('local-actions');
    LRule.Add(NewNyxButton('local-button').Node);
    LForm := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    Check(RowAction(LForm, 'actions').Prop(NyxAttributeName(atEnabled)) = 'false',
      'An owned content payload disables Hide visibly');
    Refuse('actions', npaHide, 'Hide cannot silently discard custom content');
    LDocument.Find('first').Remove(LRule);
    LRule := LDocument.Find('first').OverridePart(NyxPart('title')).Named('local-title');
    LRule.SetProp(NyxAttributeName(atText), 'Local title');
    LForm := NewNyxPartsEditor('parts', LDocument, NyxControl('first'));
    Check(RowAction(LForm, 'title').Prop(NyxAttributeName(atEnabled)) = 'false',
      'Existing noRemove rules expose local-property refusal without stripping fields');
    Refuse('title', npaHide, 'Hide cannot discard a local property override');
    LDocument.Find('first').Remove(LRule);
    Check((LPrior.Path.Name = 'actions') and (LPrior.Baseline = LPriorBaseline),
      'Refusals retain the independent copied proposal');
  finally
    LCollision.Free;
    LDocument.Free;
  end;
end;

{ Stored descriptor order remains the composition owner's authority. A later
  parent removal can conceal an earlier child customization without deleting
  it; moving that removal before the child makes the same path invalid. }
procedure OrderedOverrides;
var
  LDocument, LHidden, LDecoded, LRestored: TNyxDocument;
  LRule, LRuntime: TNyxNode;
  LForm: INyxCard;
  LHide, LRestore: TNyxPartsEditorProposal;
  LBefore, LChild, LDefinition, LOther: TNyxText;
  LFailed: Boolean;
begin
  LDocument := Fixture;
  LHidden := nil;
  LDecoded := nil;
  LRestored := nil;
  LRuntime := nil;
  try
    LRule := LDocument.Find('first').OverridePart(NyxPart('actions/send')).Named('nested-rule');
    LRule.SetProp(NyxAttributeName(atText), 'Local send');
    LBefore := TNyxCodec.Encode(LDocument);
    LChild := TNyxCodec.NodeSnapshot(LRule).ToJSON;
    LDefinition := TNyxCodec.NodeSnapshot(LDocument.Find('card-definition')).ToJSON;
    LOther := TNyxCodec.NodeSnapshot(LDocument.Find('second')).ToJSON;
    LHide := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('actions'), npaHide);
    LHidden := Candidate(LDocument, LHide);
    Check(not HasPart(LHidden, 'first', 'actions') and HasPart(LHidden, 'second', 'actions/send'),
      'A new later parent removal hides only the selected instance subtree');
    Check((TNyxCodec.NodeSnapshot(LHidden.Find('nested-rule')).ToJSON = LChild) and
      (TNyxCodec.NodeSnapshot(LHidden.Find('card-definition')).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LHidden.Find('second')).ToJSON = LOther) and
      (TNyxCodec.Encode(LDocument) = LBefore),
      'Child-first hiding preserves the exact child descriptor, definition, sibling and input');
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LHidden));
    Check(TNyxCodec.NodeSnapshot(LDecoded.Find('nested-rule')).ToJSON = LChild,
      'Saved hidden data retains the complete earlier child customization');
    LForm := NewNyxPartsEditor('ordered-parts', LDecoded, NyxControl('first'));
    Check(CaptureNyxPartsEditor(RowAction(LForm, 'actions'), LForm.Node, LDecoded, LRestore) and
      (LRestore.Descriptor.ID = LHide.Descriptor.ID),
      'The hidden parent retains its own exact Restore descriptor beside the child rule');
    LRestored := Candidate(LDecoded, LRestore);
    Check(TNyxCodec.Encode(LRestored) = LBefore,
      'Restore removes only the parent removal and returns the exact customized document');
    LRuntime := RealizeNyxView(LRestored, LRestored.Find('first'));
    Check(LRuntime.Part(NyxPart('actions/send')).Prop(NyxAttributeName(atText)) = 'Local send',
      'Restored realization still applies the preserved child customization');
    FreeAndNil(LRuntime);

    LDocument.Find('first').Remove(LRule);
    LDocument.Find('first').OverridePart(NyxPart('actions')).Named('parent-rule');
    LDocument.Find('first').OverridePart(NyxPart('actions/send')).Named('nested-rule')
      .SetProp(NyxAttributeName(atText), 'Local send');
    LBefore := TNyxCodec.Encode(LDocument);
    LFailed := False;
    try
      NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('actions'), npaHide);
    except
      on ENyxModel do LFailed := True;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'Changing an earlier parent rule to removal refuses when a later child path would become invalid');
  finally
    LRuntime.Free;
    LRestored.Free;
    LDecoded.Free;
    LHidden.Free;
    LDocument.Free;
  end;
end;

procedure RetainedForm;
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LProposal: TNyxPartsEditorProposal;
  LFailed: Boolean;
begin
  LDocument := Fixture;
  try
    LForm := NewNyxPartsEditor('retained', LDocument, NyxControl('first'));
    LProposal := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('title'), npaHide);
  finally
    LDocument.Free;
  end;
  Check((RowAction(LForm, 'actions') <> nil) and (LProposal.Instance.ID = 'first'),
    'Published form and copied proposal outlive their borrowed document without a back-reference');
  LFailed := False;
  try CaptureNyxPartsEditor(RowAction(LForm, 'actions'), LForm.Node, nil, LProposal);
  except on ENyxModel do LFailed := True; end;
  Check(LFailed, 'A retained form needs a current owning document before capture');
end;

{ Focus follows presentation semantics, never an old ordinal or borrowed node.
  The same real remove/inherit candidates drive both action transitions. }
procedure FocusAnchors;
var
  LDocument, LHidden, LRestored: TNyxDocument;
  LForm, LHiddenForm, LRestoredForm, LOtherForm, LMalformed: INyxCard;
  LButton: TNyxNode;
  LFocus, LUnicode, LChild, LRestoreFocus, LRejected: TNyxPartsEditorFocus;
  LProposal: TNyxPartsEditorProposal;
  LBefore, LFormerID, LOrdinal, LVersion, LAction: TNyxText;
begin
  LDocument := Fixture;
  LHidden := nil;
  LRestored := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LForm := NewNyxPartsEditor('focus-parts', LDocument, NyxControl('first'));
    LButton := RowAction(LForm, 'actions');
    LFormerID := LButton.ID;
    Check(TryCaptureNyxPartsEditorFocus(LButton, LForm.Node, LFocus) and LFocus.Defined and
      (LFocus.FormID = 'focus-parts') and (LFocus.Instance.ID = 'first') and
      (LFocus.Path.Name = 'actions'), 'A live action copies only its exact form, instance and named path');
    Check(LFocus.Resolve(LForm.Node).ID = LButton.ID,
      'The copied focus identity resolves the current action before publication');
    Check(TryCaptureNyxPartsEditorFocus(RowAction(LForm, '标题🌙<&>'), LForm.Node, LUnicode),
      'A Unicode path has an independent copied presentation identity');
    Check(TryCaptureNyxPartsEditorFocus(RowAction(LForm, 'actions/send'), LForm.Node, LChild),
      'A nested part has its own exact focus identity');
    LProposal := NyxPartsEditorProposal(LDocument, NyxControl('first'), NyxPart('actions'), npaHide);
    LHidden := Candidate(LDocument, LProposal);
    LHiddenForm := NewNyxPartsEditor('focus-parts', LHidden, NyxControl('first'));
    Check(RowAction(LHiddenForm, '标题🌙<&>').ID = LFormerID,
      'The real hidden-parent form reuses the former Hide ordinal for a different path');
    Check((LFocus.Resolve(LHiddenForm.Node).ID = RowAction(LHiddenForm, 'actions').ID) and
      (LFocus.Resolve(LHiddenForm.Node).ID <> LFormerID),
      'Hide resolves the same path''s new Restore action instead of the reused ordinal');
    Check(LUnicode.Resolve(LHiddenForm.Node).ID = RowAction(LHiddenForm, '标题🌙<&>').ID,
      'The unaffected Unicode path follows its reordered current action');
    Check(LChild.Resolve(LHiddenForm.Node).ID = '',
      'A vanished descendant has no guessed parent or ordinal focus target');
    Check(TryCaptureNyxPartsEditorFocus(RowAction(LHiddenForm, 'actions'), LHiddenForm.Node,
      LRestoreFocus), 'A hidden path''s Restore action can supply a new focus identity');
    LProposal := NyxPartsEditorProposal(LHidden, NyxControl('first'), NyxPart('actions'), npaRestore);
    LRestored := Candidate(LHidden, LProposal);
    LRestoredForm := NewNyxPartsEditor('focus-parts', LRestored, NyxControl('first'));
    Check((LRestoreFocus.Resolve(LRestoredForm.Node).ID = RowAction(LRestoredForm, 'actions').ID) and
      (LFocus.Resolve(LRestoredForm.Node).ID = RowAction(LRestoredForm, 'actions').ID),
      'Restore and Undo-style reconstruction resolve the current same-path Hide action');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'Focus capture and resolution do not mutate source data');

    LOtherForm := NewNyxPartsEditor('focus-parts', LDocument, NyxControl('second'));
    Check(LFocus.Resolve(LOtherForm.Node).ID = '',
      'A same-ID form for a different selected instance has no inherited focus target');
    Check(not TryCaptureNyxPartsEditorFocus(RowAction(LOtherForm, 'actions'), LForm.Node, LRejected) and
      not LRejected.Defined, 'A same-ID action borrowed from another form refuses and clears capture');
    LOtherForm := NewNyxPartsEditor('other-form', LDocument, NyxControl('first'));
    Check(LFocus.Resolve(LOtherForm.Node).ID = '', 'A different form cannot satisfy the copied identity');
    Check((LFocus.Resolve(nil).ID = '') and
      not TryCaptureNyxPartsEditorFocus(nil, nil, LRejected) and not LRejected.Defined,
      'Missing roots and unrelated input remain inert presentation refusals');
    Check(not TryCaptureNyxPartsEditorFocus(LForm.Node.Find('focus-parts-help'), LForm.Node, LRejected),
      'A neighboring label cannot become a Parts focus action');

    LMalformed := NewNyxPartsEditor('focus-parts', LDocument, NyxControl('first'));
    LVersion := LMalformed.Node.Prop('nyx.parts-editor.version');
    LMalformed.Node.SetProp('nyx.parts-editor.version', 'unknown');
    Check((LFocus.Resolve(LMalformed.Node).ID = '') and
      not TryCaptureNyxPartsEditorFocus(RowAction(LMalformed, 'actions'), LMalformed.Node, LRejected),
      'An unknown form version refuses both capture and resolution');
    LMalformed.Node.SetProp('nyx.parts-editor.version', LVersion);
    LButton := RowAction(LMalformed, 'actions');
    LOrdinal := LButton.Prop('nyx.parts-editor.row');
    LAction := LButton.Prop('nyx.parts-editor.action');
    LButton.SetProp('nyx.parts-editor.row', '0' + LOrdinal);
    Check((LFocus.Resolve(LMalformed.Node).ID = '') and
      not TryCaptureNyxPartsEditorFocus(LButton, LMalformed.Node, LRejected),
      'Noncanonical row metadata cannot redirect a focus identity');
    LButton.SetProp('nyx.parts-editor.row', LOrdinal).SetProp('nyx.parts-editor.action', 'unknown');
    Check(not TryCaptureNyxPartsEditorFocus(LButton, LMalformed.Node, LRejected) and not LRejected.Defined,
      'An unknown action leaves no captured focus identity');
    LButton.SetProp('nyx.parts-editor.action', LAction).SetProp('nyx.parts-editor.path', '/');
    Check(not TryCaptureNyxPartsEditorFocus(LButton, LMalformed.Node, LRejected),
      'Malformed named paths refuse without interpreting their text');
    LButton.SetProp('nyx.parts-editor.path', 'actions');
    LButton.Configure.Enabled(False).Done;
    Check((LFocus.Resolve(LMalformed.Node).ID = '') and
      not TryCaptureNyxPartsEditorFocus(LButton, LMalformed.Node, LRejected),
      'An unavailable action is not a physical focus continuation');
    LButton.Configure.Enabled(True).Done;
    RowAction(LMalformed, '标题🌙<&>').Parent.SetProp('nyx.parts-editor.path', 'actions');
    Check(LFocus.Resolve(LMalformed.Node).ID = '', 'Ambiguous current path rows yield no focus target');

    LForm := nil;
    FreeAndNil(LDocument);
    Check(LFocus.Defined and (LFocus.Instance.ID = 'first') and (LFocus.Path.Name = 'actions') and
      (LFocus.Resolve(LRestoredForm.Node).ID = RowAction(LRestoredForm, 'actions').ID),
      'A copied focus identity outlives its source form and document without a back-reference');
  finally
    LRestored.Free;
    LHidden.Free;
    LDocument.Free;
  end;
end;

begin
  try
    HideRestore;
    Refusals;
    OrderedOverrides;
    RetainedForm;
    FocusAnchors;
    WriteLn('PASS ', GChecks, ' public Parts editor checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-parts-editor', 'passed');
    document.body.setAttribute('data-parts-checks', IntToStr(GChecks));
    {$endif}
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-parts-editor', 'failed');
      document.body.setAttribute('data-parts-checks', IntToStr(GChecks));
      document.body.setAttribute('data-parts-error', E.ClassName + ': ' + E.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
end.
