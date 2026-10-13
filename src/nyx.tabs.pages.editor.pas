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

unit nyx.tabs.pages.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls;

type
  TNyxTabPageOperation = (ntpAdd, ntpRename, ntpEarlier, ntpLater, ntpRemove);
  TNyxTabPagesField = (ntfPage, ntfCaption, ntfNewCaption, ntfConfirm,
    ntfAdd, ntfRename, ntfEarlier, ntfLater, ntfRemove, ntfDefinition);
  { Value-only intent. Exact authored subtree plus effective choices form the
    baseline; queued source admission checks the complete pair independently.
    Removal confirms the entire page and descendants, without rewriting any
    selection, binding, default, handler or external reference. }
  TNyxTabPageChange = record
    { Exact authored tabs owner and direct page; Page is empty for creation. }
    Owner: TNyxControlRef;
    Page: TNyxControlRef;
    Operation: TNyxTabPageOperation;
    { Application text for Add/Rename; other operations preserve existing text. }
    Caption: TNyxText;
    { Independent exact subtree/choice data, never an execution authority. }
    Baseline: TNyxText;
    { Removal requires acknowledgement of this captured page and its contents. }
    Confirmed: Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTabPageChange; static;
  end;
  { Ephemeral form state owns strings only. Absent panels park the draft;
    changed owner/subtree/effective context retires it before restoration. }
  TNyxTabPagesDraft = record
  private
    FEditor: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FValues: array[ntfPage..ntfConfirm] of TNyxText;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
  end;

{ Stable part identity from a closed field vocabulary. }
function NyxTabPagesFieldID(const AEditor: TNyxText; AField: TNyxTabPagesField): TNyxText;
{ Exact copied authored subtree and realized direct choices. Requires tabs or a
  reusable reference projecting as tabs. No runtime control/document is retained. }
function NyxTabPagesBaseline(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxText;
{ Ordinary specialized Nyx parts. Owned tabs expose structural commands. A
  reusable instance exposes its definition button instead; hosts decide how to
  navigate to that exact definition without changing the instance or document.
  Input parts are page/caption/new-caption/confirm; command parts live under
  actions/add, actions/rename, actions/earlier, actions/later and actions/remove. }
function NewNyxTabPagesEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated buttons return False. Captures only exact mounted parts and copied
  identities; known action/shape/choice/confirmation failures raise before edit. }
function CaptureNyxTabPagesEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxTabPageChange): Boolean;
{ Definition navigation is a distinct reference, never an inferred button name. }
function NyxTabPagesDefinition(AEditor: TNyxNode): TNyxControlRef;
{ Create an independently owned complete candidate. Exact subtree/effective
  baseline and direct membership precede mutation; full document/property
  admission follows it. Any failure frees the candidate and leaves source intact. }
function EditNyxTabPages(ADocument: TNyxDocument;
  const AChange: TNyxTabPageChange): TNyxDocument;
{ Prefill caption from the selected authored page and revoke old removal consent.
  Changes only this disposable form; caller synchronizes its retained controls. }
function RouteNyxTabPageChoice(ANode, ARoot: TNyxNode): Boolean;

implementation

uses
  SysUtils, nyx.codec, nyx.tabs, nyx.tabs.editor, nyx.menu.editor,
  nyx.schema, nyx.composition, nyx.binding.types;

const
  CEditor = 'nyx.tab-pages-editor';
  COwner = 'nyx.tab-pages-editor.owner';
  CBaseline = 'nyx.tab-pages-editor.baseline';
  CDefinition = 'nyx.tab-pages-editor.definition';
  CFields: array[TNyxTabPagesField] of TNyxText =
    ('page', 'caption', 'new-caption', 'confirm', 'add', 'rename', 'earlier',
    'later', 'remove', 'definition');
  COperations: array[TNyxTabPageOperation] of TNyxText =
    ('add', 'rename', 'earlier', 'later', 'remove');

function NyxTabPagesFieldID(const AEditor: TNyxText; AField: TNyxTabPagesField): TNyxText;
begin
  Result := AEditor + TNyxText('-') + CFields[AField];
end;

function TNyxTabPageChange.ToData: TNyxDataValue;
begin

  if (Owner.ID = '') or (Baseline = '') or
    (Ord(Operation) < Ord(Low(TNyxTabPageOperation))) or
    (Ord(Operation) > Ord(High(TNyxTabPageOperation))) then
  begin
    raise ENyxModel.Create('Tab page intent requires its owner, baseline and known action');
  end;

  if (Operation <> ntpAdd) and (Page.ID = '') then
  begin
    raise ENyxModel.Create('Select an exact tab page first');
  end;

  if (Operation = ntpRemove) and not Confirmed then
  begin
    raise ENyxModel.Create('Confirm removal of this page and all of its contents first');
  end;
  Result := NyxObject([NyxField('owner', NyxData(Owner.ID)),
    NyxField('page', NyxData(Page.ID)), NyxField('action', NyxData(COperations[Operation])),
    NyxField('caption', NyxData(Caption)), NyxField('baseline', NyxData(Baseline)),
    NyxField('confirmed', NyxData(Confirmed))]);
end;

class function TNyxTabPageChange.FromData(const AData: TNyxDataValue): TNyxTabPageChange;
var
  LOperation: TNyxTabPageOperation;
  LFound: Boolean;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 6) then
  begin
    raise ENyxModel.Create('Tab page intent requires six exact fields');
  end;
  Result := Default(TNyxTabPageChange);
  Result.Owner := NyxControl(AData.Field('owner').AsText);
  Result.Page := NyxControl(AData.Field('page').AsText);
  Result.Caption := AData.Field('caption').AsText;
  Result.Baseline := AData.Field('baseline').AsText;
  Result.Confirmed := AData.Field('confirmed').AsBoolean;
  LFound := False;
  for LOperation := Low(TNyxTabPageOperation) to High(TNyxTabPageOperation) do
  begin

    if AData.Field('action').AsText = COperations[LOperation] then
    begin
      Result.Operation := LOperation;
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Unknown tab page action');
  end;
  Result.ToData;
end;

function NyxTabPagesBaseline(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
  LPolicy: TNyxDataValue;
begin
  LPolicy := TNyxDataValue.ParseJSON(NyxTabsEditorBaseline(ADocument, AOwner));
  LOwner := ADocument.Find(AOwner.ID);
  Result := NyxObject([NyxField('authored', TNyxCodec.NodeSnapshot(LOwner)),
    NyxField('choices', LPolicy.Field('children'))]).ToJSON;
end;

function Choice(const AChildren: TNyxDataValue; AIndex: Integer): TNyxText;
begin
  Result := NyxMenuEditorChoiceCaption(AIndex, AChildren.Item(AIndex).Field('caption').AsText);
end;

function NyxTabPagesDefinition(AEditor: TNyxNode): TNyxControlRef;
begin
  Result := Default(TNyxControlRef);

  if AEditor <> nil then
  begin
    Result := NyxControl(AEditor.Prop(CDefinition));
  end;
end;

function NewNyxTabPagesEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxDataValue;
  LOwner: TNyxNode;
  LChildren: TNyxDataValue;
  LItems: TNyxText;
  LIndex: Integer;
  LField: TNyxTabPagesField;
  LButton: INyxButton;
  LDefinition: TNyxNode;
  LCaption: TNyxText;
  LActions: INyxRow;
begin
  LBaseline := TNyxDataValue.ParseJSON(NyxTabPagesBaseline(ADocument, AOwner));
  LOwner := ADocument.Find(AOwner.ID);
  Result := NewNyxCard(AID);
  Result.Configure.Gap(10).Padding(16).Done;
  Result.Node.SetProp(COwner, AOwner.ID).SetProp(CBaseline, LBaseline.ToJSON);
  Result.Add(NewNyxLabel(AID + TNyxText('-title')).Configure
    .PartName(NyxPart('title')).Text('Tab pages').Done);

  if LOwner.Kind = 'component' then
  begin
    LDefinition := ADocument.Find(LOwner.Prop('component'));

    if LDefinition = nil then
    begin
      raise ENyxModel.Create('This reusable tabs definition is missing');
    end;
    Result.Node.SetProp(CDefinition, LDefinition.ID);
    Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure
      .Text('These pages belong to the reusable definition. Edit it to update every instance.').Done);
    Result.Add(NewNyxButton(NyxTabPagesFieldID(AID, ntfDefinition)).Configure
      .PartName(NyxPart(CFields[ntfDefinition])).Text('Edit reusable tab pages').Done);
    Exit;
  end;
  LChildren := LBaseline.Field('choices');
  LItems := '';
  for LIndex := 0 to LChildren.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + Choice(LChildren, LIndex);
  end;
  LCaption := '';

  if LOwner.Count > 0 then
  begin
    LCaption := LOwner.Children[0].Prop(NyxAttributeName(atText));
  end;
  Result.Add(NewNyxSelect(NyxTabPagesFieldID(AID, ntfPage)).Configure
    .PartName(NyxPart(CFields[ntfPage])).Text('Page').Items(LItems).Done);

  if LChildren.Count > 0 then
  begin
    Result.Part(NyxPart(CFields[ntfPage])).Configure.Value(Choice(LChildren, 0)).Done;
  end;
  Result.Part(NyxPart(CFields[ntfPage])).Node.SetProp(CEditor, AID);
  Result.Add(NewNyxInput(NyxTabPagesFieldID(AID, ntfCaption)).Configure
    .PartName(NyxPart(CFields[ntfCaption])).Text('Page caption').Value(LCaption).Done);
  Result.Add(NewNyxInput(NyxTabPagesFieldID(AID, ntfNewCaption)).Configure
    .PartName(NyxPart(CFields[ntfNewCaption])).Text('New page caption').Value('New tab').Done);
  Result.Add(NewNyxCheckbox(NyxTabPagesFieldID(AID, ntfConfirm)).Configure
    .PartName(NyxPart(CFields[ntfConfirm]))
    .Text('I understand that removal deletes the selected page and all of its contents.').Value(False).Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-warning')).Configure
    .PartName(NyxPart('warning'))
    .Text('Resolve initial selection, state bindings and other references before removing a page. Undo restores the complete pair.').Done);
  LActions := NewNyxRow(AID + TNyxText('-actions'));
  LActions.Configure.PartName(NyxPart('actions')).Layout(nlRow).Wrap(nfwWrap).Gap(8).Done;
  Result.Add(LActions);
  for LField := ntfAdd to ntfRemove do
  begin
    LButton := NewNyxButton(NyxTabPagesFieldID(AID, LField));
    LButton.Configure.PartName(NyxPart(CFields[LField])).Done;
    case LField of
      ntfAdd:
      begin
        LCaption := 'Add page';
      end;
      ntfRename:
      begin
        LCaption := 'Rename page';
      end;
      ntfEarlier:
      begin
        LCaption := 'Move earlier';
      end;
      ntfLater:
      begin
        LCaption := 'Move later';
      end;
      ntfRemove:
      begin
        LCaption := 'Remove page';
      end;
      else
      begin
        raise ENyxModel.Create('Unknown tab page button');
      end;
    end;
    LButton.Configure.Text(LCaption).Enabled((LField = ntfAdd) or (LChildren.Count > 0)).Done;
    LButton.Node.SetProp(CEditor, AID);
    LActions.Add(LButton);
  end;
end;

function FormField(AEditor: TNyxNode; AField: TNyxTabPagesField): TNyxNode;
var
  LKind: TNyxKind;
begin
  Result := AEditor.Part(NyxPart(CFields[AField]));
  LKind := nkInput;

  if AField = ntfPage then
  begin
    LKind := nkSelect;
  end
  else if AField = ntfConfirm then
  begin
    LKind := nkCheckbox;
  end;

  if (Result = nil) or (Result.ProjectionKind <> NyxKindName(LKind)) then
  begin
    raise ENyxModel.Create('The mounted tab pages form is incomplete');
  end;
end;

function RouteNyxTabPageChoice(ANode, ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LID: TNyxText;
  LBaseline: TNyxDataValue;
  LChoices: TNyxDataValue;
  LPages: TNyxDataValue;
  LIndex: Integer;
  LProperty: Integer;
  LCaption: TNyxText;
  LProps: TNyxDataValue;
begin
  Result := False;

  if (ANode = nil) or (ANode.Prop(CEditor) = '') or
    (ANode.SourceID <> NyxTabPagesFieldID(ANode.Prop(CEditor), ntfPage)) then
  begin
    Exit;
  end;

  if (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxModel.Create('This tab page choice belongs to a retired view');
  end;
  LID := ANode.Prop(CEditor);
  LEditor := ANode.Parent;
  while (LEditor <> nil) and (LEditor.SourceID <> LID) do
  begin
    LEditor := LEditor.Parent;
  end;

  if LEditor = nil then
  begin
    raise ENyxModel.Create('The tab pages form is absent');
  end;
  LBaseline := TNyxDataValue.ParseJSON(LEditor.Prop(CBaseline));
  LChoices := LBaseline.Field('choices');
  LPages := LBaseline.Field('authored').Field('children');
  for LIndex := 0 to LChoices.Count - 1 do
  begin

    if ANode.Prop(NyxAttributeName(atValue)) = Choice(LChoices, LIndex) then
    begin
      LCaption := '';
      LProps := LPages.Item(LIndex).Field('props');
      for LProperty := 0 to LProps.Count - 1 do
      begin

        if LProps.Key(LProperty) = NyxAttributeName(atText) then
        begin
          LCaption := LProps.Field(NyxAttributeName(atText)).AsText;
        end;
      end;
      FormField(LEditor, ntfCaption).SetProp(NyxAttributeName(atValue), LCaption);
      FormField(LEditor, ntfConfirm).SetProp(NyxAttributeName(atValue), 'false');
      Exit(True);
    end;
  end;
  raise ENyxModel.Create('This choice is not an exact tab page');
end;

function EditNyxTabPages(ADocument: TNyxDocument;
  const AChange: TNyxTabPageChange): TNyxDocument;
var
  LOwner: TNyxNode;
  LPage: TNyxNode;
  LIndex: Integer;
  LNumber: Integer;
  LID: TNyxText;
  LNewPage: INyxTab;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LBinding: TNyxBindingSpec;
begin
  AChange.ToData;

  if NyxTabPagesBaseline(ADocument, AChange.Owner) <> AChange.Baseline then
  begin
    raise ENyxModel.Create('Tab pages or their contents changed since this intent was captured');
  end;
  Result := ADocument.Clone;
  try
    LOwner := Result.Find(AChange.Owner.ID);

    if (LOwner.Kind = 'component') or (LOwner.ProjectionKind <> 'tabs') then
    begin
      raise ENyxModel.Create('Edit the reusable definition to change its tab pages');
    end;
    LPage := nil;
    LIndex := -1;

    if AChange.Operation <> ntpAdd then
    begin
      for LNumber := 0 to LOwner.Count - 1 do
      begin

        if LOwner.Children[LNumber].ID = AChange.Page.ID then
        begin
          LPage := LOwner.Children[LNumber];
          LIndex := LNumber;
        end;
      end;

      if LPage = nil then
      begin
        raise ENyxModel.Create('The requested page is not owned by this tabs component');
      end;
    end;
    case AChange.Operation of
      ntpAdd:
      begin
        LNumber := 0;
        repeat
          Inc(LNumber);
          LID := LOwner.ID + TNyxText('-page-') + TNyxText(IntToStr(LNumber));
        until Result.Find(LID) = nil;
        LNewPage := NewNyxTab(LID);
        LNewPage.Configure.Text(AChange.Caption).Layout(nlColumn).Gap(12).Padding(16).Done;
        LOwner.Add(LNewPage);
      end;
      ntpRename:
      begin
        LRoot := RealizeNyxContext(Result, LPage, LProjection);
        try

          if (LProjection <> nil) and LProjection.FindBinding(bpText, LBinding) then
          begin
            raise ENyxModel.Create('This caption is bound. Edit its text binding or default instead');
          end;
        finally
          LRoot.Free;
        end;
        LPage.Configure.Text(AChange.Caption).Done;
      end;
      ntpEarlier, ntpLater:
      begin

        if AChange.Operation = ntpEarlier then
        begin
          Dec(LIndex);
        end
        else
        begin
          Inc(LIndex);
        end;

        if (LIndex < 0) or (LIndex >= LOwner.Count) then
        begin
          raise ENyxModel.Create('This page is already at that edge');
        end;
        Result.MoveControl(AChange.Page, AChange.Owner, LIndex);
      end;
      ntpRemove:
      begin
        LOwner.Remove(LPage);
      end;
    end;
    Result.Validate;
    ValidateNyxDocumentProperties(Result);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

function CaptureNyxTabPagesEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxTabPageChange): Boolean;
var
  LEditor: TNyxNode;
  LID: TNyxText;
  LField: TNyxTabPagesField;
  LChildren: TNyxDataValue;
  LIndex: Integer;
  LChoice: TNyxText;
  LFound: Boolean;
begin
  Result := False;
  AChange := Default(TNyxTabPageChange);

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditor);

  if (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('This tab pages form belongs to a retired view');
  end;
  LEditor := AButton.Parent;
  while (LEditor <> nil) and (LEditor.SourceID <> LID) do
  begin
    LEditor := LEditor.Parent;
  end;

  if LEditor = nil then
  begin
    raise ENyxModel.Create('This tab pages form is no longer mounted');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwner));
  AChange.Baseline := LEditor.Prop(CBaseline);
  LFound := False;
  for LField := ntfAdd to ntfRemove do
  begin

    if AButton.SourceID = NyxTabPagesFieldID(LID, LField) then
    begin
      AChange.Operation := TNyxTabPageOperation(Ord(LField) - Ord(ntfAdd));
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Unknown tab pages form action');
  end;
  AChange.Caption := FormField(LEditor, ntfNewCaption).Prop(NyxAttributeName(atValue));

  if AChange.Operation <> ntpAdd then
  begin
    LChildren := TNyxDataValue.ParseJSON(AChange.Baseline).Field('choices');
    LChoice := FormField(LEditor, ntfPage).Prop(NyxAttributeName(atValue));
    LFound := False;
    for LIndex := 0 to LChildren.Count - 1 do
    begin

      if LChoice = Choice(LChildren, LIndex) then
      begin
        AChange.Page := NyxControl(LChildren.Item(LIndex).Field('id').AsText);
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise ENyxModel.Create('Select one of this form''s exact tab pages');
    end;
    AChange.Caption := FormField(LEditor, ntfCaption).Prop(NyxAttributeName(atValue));
  end;
  LChoice := FormField(LEditor, ntfConfirm).Prop(NyxAttributeName(atValue));

  if (LChoice <> 'true') and (LChoice <> 'false') then
  begin
    raise ENyxModel.Create('Removal confirmation requires an exact Boolean');
  end;
  AChange.Confirmed := LChoice = 'true';
  AChange.ToData;
  Result := True;
end;

procedure TNyxTabPagesDraft.Clear;
begin
  Self := Default(TNyxTabPagesDraft);
end;

procedure TNyxTabPagesDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LField: TNyxTabPagesField;
  LNext: TNyxTabPagesDraft;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AEditor);

  if (LEditor = nil) or (NyxTabPagesDefinition(LEditor).ID <> '') then
  begin
    Exit;
  end;
  LNext := Default(TNyxTabPagesDraft);
  LNext.FEditor := AEditor;
  LNext.FOwner := LEditor.Prop(COwner);
  LNext.FBaseline := LEditor.Prop(CBaseline);
  for LField := ntfPage to ntfConfirm do
  begin
    LNext.FValues[LField] := FormField(LEditor, LField).Prop(NyxAttributeName(atValue));
  end;
  Self := LNext;
end;

function TNyxTabPagesDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxTabPagesField;
begin
  Result := False;

  if (FEditor = '') or (ARoot = nil) then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(FEditor);

  if LEditor = nil then
  begin
    Exit;
  end;

  if (LEditor.Prop(COwner) <> FOwner) or (LEditor.Prop(CBaseline) <> FBaseline) then
  begin
    Clear;
    Exit;
  end;
  for LField := ntfPage to ntfConfirm do
  begin
    FormField(LEditor, LField);
  end;
  for LField := ntfPage to ntfConfirm do
  begin
    FormField(LEditor, LField).SetProp(NyxAttributeName(atValue), FValues[LField]);
  end;
  Result := True;
end;

end.
