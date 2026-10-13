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

unit nyx.selection.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.selection;

type
  { Closed public parts of the reusable appearance form. }
  TNyxSelectionEditorField = (nsfStyling, nsfSelected, nsfUnselected, nsfApply, nsfInherit);
  { Value-only proposal. Baseline captures the exact local/effective policy,
    scalar domain and direct selection parts. Inherit removes only the local
    declaration. The receiver revalidates before one complete paired command. }
  TNyxSelectionEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Inherit: Boolean;
    Options: TNyxSelectionOptions;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxSelectionEditorChange; static;
  end;
  { Copied disposable inputs, without arrays or borrowed source/view references.
    Scalar storage avoids fixed-array aliasing when pas2js copies project state.
    Missing panels retain the draft; a changed owner/baseline retires it. }
  TNyxSelectionEditorDraft = record
  private
    FEditor: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FStyling: TNyxText;
    FSelected: TNyxText;
    FUnselected: TNyxText;
  public
    procedure Clear;
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
  end;

{ Physical/editor identity is independent of the displayed field caption. }
function NyxSelectionEditorFieldID(const AEditor: TNyxText;
  AField: TNyxSelectionEditorField): TNyxText;
{ Borrow only during inspection. Complete independent realization admits scalar
  applicability and copies an exact baseline; no source nodes survive the call. }
function NyxSelectionEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ Public Nyx Card/Select/Button composition. Invalid owners refuse before a form
  is returned. The form changes no source or document; creators may customize it. }
function NewNyxSelectionEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated buttons return False. Exact view membership and typed choice decoding
  precede the copied proposal. This does not mutate a source tree or history. }
function CaptureNyxSelectionEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxSelectionEditorChange): Boolean;

implementation

uses
  nyx.composition, nyx.contract, nyx.schema;

const
  CEditor = 'nyx.selection-editor';
  COwner = 'nyx.selection-editor.owner';
  CBaseline = 'nyx.selection-editor.baseline';
  CFields: array[TNyxSelectionEditorField] of TNyxText =
    ('styling', 'selected', 'unselected', 'apply', 'inherit');
  CStyling: array[TNyxSelectionStyling] of TNyxText =
    ('Use selected and unselected variants', 'Keep each part''s authored variant');
  CVariants: array[TNyxVariant] of TNyxText =
    ('Default', 'Primary', 'Secondary', 'Danger', 'Success', 'Warning', 'Ghost');

function NyxSelectionEditorFieldID(const AEditor: TNyxText;
  AField: TNyxSelectionEditorField): TNyxText;
begin
  Result := AEditor + '-' + CFields[AField];
end;

function TNyxSelectionEditorChange.ToData: TNyxDataValue;
var
  LOptions: TNyxDataValue;
begin
  NyxControl(Owner.ID);

  if Baseline = '' then
  begin
    raise ENyxModel.Create('Selection editor intent requires its exact baseline');
  end;
  LOptions := NyxNull;

  if not Inherit then
  begin
    LOptions := Options.ToData;
  end;
  Result := NyxObject([NyxField('owner', NyxData(Owner.ID)),
    NyxField('baseline', NyxData(Baseline)), NyxField('inherit', NyxData(Inherit)),
    NyxField('options', LOptions)]);
end;

class function TNyxSelectionEditorChange.FromData(const AData: TNyxDataValue):
  TNyxSelectionEditorChange;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 4) then
  begin
    raise ENyxModel.Create('Selection editor intent requires four exact fields');
  end;
  Result := Default(TNyxSelectionEditorChange);
  Result.Owner := NyxControl(AData.Field('owner').AsText);
  Result.Baseline := AData.Field('baseline').AsText;
  Result.Inherit := AData.Field('inherit').AsBoolean;
  Result.Options := TNyxSelectionOptions.Defaults;

  if Result.Inherit then
  begin

    if AData.Field('options').Kind <> ndNull then
    begin
      raise ENyxModel.Create('Inherited selection policy cannot contain options');
    end;
  end
  else
  begin
    Result.Options := TNyxSelectionOptions.FromData(AData.Field('options'));
  end;
  Result.ToData;
end;

function NyxSelectionEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
  LProjection: TNyxNode;
  LContext: TNyxNode;
  LLocal: TNyxDataValue;
  LParts: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LPart: TNyxNode;
  LDomain: TNyxValueDomain;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Selection editor requires its document');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('The selection editor owner is absent');
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LProjection);
  try
    LDomain := NyxNodeValueDomain(LProjection);

    if not LDomain.Defined then
    begin
      raise ENyxModel.Create('Selection appearance requires a scalar compound');
    end;
    SetLength(LParts, LProjection.Count);
    LCount := 0;
    for LIndex := 0 to LProjection.Count - 1 do
    begin
      LPart := LProjection.Children[LIndex];

      if LPart.Prop(NyxAttributeName(atAction)) = NyxActionName(naSelect) then
      begin
        LParts[LCount] := NyxObject([
          NyxField('id', NyxData(LPart.SourceID)),
          NyxField('kind', NyxData(LPart.ProjectionKind)),
          NyxField('part', NyxData(LPart.Prop(NyxAttributeName(atPart)))),
          NyxField('option', NyxData(LPart.Prop(NyxAttributeName(atOption)))),
          NyxField('variant', NyxData(LPart.Prop(NyxAttributeName(atVariant))))]);
        Inc(LCount);
      end;
    end;

    if LCount = 0 then
    begin
      raise ENyxModel.Create('Selection appearance requires direct selection parts');
    end;
    SetLength(LParts, LCount);
    LLocal := NyxNull;

    if LOwner.Extensions.Has(NyxExtension(NyxSelectionOptionsKey)) then
    begin
      LLocal := LOwner.Extensions.Value(NyxExtension(NyxSelectionOptionsKey));
    end;
    Result := NyxObject([NyxField('local', LLocal),
      NyxField('effective', NyxSelectionOptions(LProjection).ToData),
      NyxField('domain', LDomain.ToData), NyxField('parts', NyxArray(LParts))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

function NewNyxSelectionEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxDataValue;
  LOptions: TNyxSelectionOptions;
  LItems: TNyxText;
  LVariant: TNyxVariant;
  LButton: INyxButton;
  LField: TNyxSelectionEditorField;
begin
  LBaseline := TNyxDataValue.ParseJSON(NyxSelectionEditorBaseline(ADocument, AOwner));
  LOptions := TNyxSelectionOptions.FromData(LBaseline.Field('effective'));
  LItems := '';
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin

    if LItems <> '' then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + CVariants[LVariant];
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Gap(10).Padding(16).Done;
  Result.Node.SetProp(COwner, AOwner.ID).SetProp(CBaseline, LBaseline.ToJSON);
  Result.Add(NewNyxLabel(AID + '-title').Configure.PartName(NyxPart('title'))
    .Text('Selection appearance').Done);
  Result.Add(NewNyxSelect(NyxSelectionEditorFieldID(AID, nsfStyling)).Configure
    .PartName(NyxPart(CFields[nsfStyling])).Text('Appearance')
    .Items(CStyling[nssVariants] + #10 + CStyling[nssAuthored])
    .Value(CStyling[LOptions.StylingMode]).Done);
  Result.Add(NewNyxSelect(NyxSelectionEditorFieldID(AID, nsfSelected)).Configure
    .PartName(NyxPart(CFields[nsfSelected])).Text('Selected variant')
    .Items(LItems).Value(CVariants[LOptions.SelectedVariant]).Done);
  Result.Add(NewNyxSelect(NyxSelectionEditorFieldID(AID, nsfUnselected)).Configure
    .PartName(NyxPart(CFields[nsfUnselected])).Text('Unselected variant')
    .Items(LItems).Value(CVariants[LOptions.UnselectedVariant]).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Authored appearance keeps each part''s variant. Selection and bindings still work.'));
  for LField := nsfApply to nsfInherit do
  begin
    LButton := NewNyxButton(NyxSelectionEditorFieldID(AID, LField));
    LButton.Configure.PartName(NyxPart(CFields[LField])).Text('Apply appearance').Done;

    if LField = nsfInherit then
    begin
      LButton.Configure.Text('Use inherited appearance')
        .Enabled(LBaseline.Field('local').Kind <> ndNull).Done;
    end;
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
end;

function FormField(AEditor: TNyxNode; AField: TNyxSelectionEditorField): TNyxNode;
begin
  Result := AEditor.Part(NyxPart(CFields[AField]));

  if Result.ProjectionKind <> NyxKindName(nkSelect) then
  begin
    raise ENyxModel.Create('The mounted selection form is incomplete');
  end;
end;

function ReadVariant(const AChoice: TNyxText): TNyxVariant;
var
  LVariant: TNyxVariant;
begin
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin

    if CVariants[LVariant] = AChoice then
    begin
      Exit(LVariant);
    end;
  end;
  raise ENyxModel.Create('Unknown selection variant choice');
end;

function CaptureNyxSelectionEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxSelectionEditorChange): Boolean;
var
  LID: TNyxText;
  LEditor: TNyxNode;
  LBaseline: TNyxDataValue;
  LChoice: TNyxText;
begin
  AChange := Default(TNyxSelectionEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditor);

  if (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('This selection form belongs to a retired view');
  end;
  LEditor := AButton.Parent;
  while (LEditor <> nil) and (LEditor.SourceID <> LID) do
  begin
    LEditor := LEditor.Parent;
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('Select this appearance form again before applying');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwner));
  AChange.Baseline := LEditor.Prop(CBaseline);
  LBaseline := TNyxDataValue.ParseJSON(AChange.Baseline);
  AChange.Inherit := AButton.SourceID = NyxSelectionEditorFieldID(LID, nsfInherit);
  AChange.Options := TNyxSelectionOptions.Defaults;

  if AChange.Inherit then
  begin

    if LBaseline.Field('local').Kind = ndNull then
    begin
      raise ENyxModel.Create('There is no local selection declaration to inherit');
    end;
    AChange.ToData;
    Exit(True);
  end;

  if AButton.SourceID <> NyxSelectionEditorFieldID(LID, nsfApply) then
  begin
    raise ENyxModel.Create('Unknown selection form action');
  end;
  LChoice := FormField(LEditor, nsfStyling).Prop('value');

  if LChoice = CStyling[nssAuthored] then
  begin
    AChange.Options := AChange.Options.Styling(nssAuthored);
  end
  else if LChoice <> CStyling[nssVariants] then
  begin
    raise ENyxModel.Create('Unknown selection styling choice');
  end;
  AChange.Options := AChange.Options
    .Selected(ReadVariant(FormField(LEditor, nsfSelected).Prop('value')))
    .Unselected(ReadVariant(FormField(LEditor, nsfUnselected).Prop('value')));
  AChange.ToData;
  Result := True;
end;

procedure TNyxSelectionEditorDraft.Clear;
begin
  FEditor := '';
  FOwner := '';
  FBaseline := '';
  FStyling := '';
  FSelected := '';
  FUnselected := '';
end;

procedure TNyxSelectionEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LNext: TNyxSelectionEditorDraft;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AEditor);

  if LEditor = nil then
  begin
    Exit;
  end;
  LNext := Default(TNyxSelectionEditorDraft);
  LNext.FEditor := AEditor;
  LNext.FOwner := LEditor.Prop(COwner);
  LNext.FBaseline := LEditor.Prop(CBaseline);
  LNext.FStyling := FormField(LEditor, nsfStyling).Prop('value');
  LNext.FSelected := FormField(LEditor, nsfSelected).Prop('value');
  LNext.FUnselected := FormField(LEditor, nsfUnselected).Prop('value');
  Self := LNext;
end;

function TNyxSelectionEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LStyling: TNyxNode;
  LSelected: TNyxNode;
  LUnselected: TNyxNode;
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
  { Admit all fields before restoring any disposable value. }
  LStyling := FormField(LEditor, nsfStyling);
  LSelected := FormField(LEditor, nsfSelected);
  LUnselected := FormField(LEditor, nsfUnselected);
  LStyling.Configure.Value(FStyling).Done;
  LSelected.Configure.Value(FSelected).Done;
  LUnselected.Configure.Value(FUnselected).Done;
  Result := True;
end;

end.
