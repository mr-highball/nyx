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

unit nyx.tabs.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.tabs;

type
  { Closed names identify the public compound's ordinary Nyx parts. }
  TNyxTabsEditorField = (nteInitial, nteActivation, nteOrientation, nteWrap,
    nteApply, nteInherit);
  { Copied intent. Inherit removes only a local declaration; explicit Defaults
    remains distinct. Baseline owns local/effective policy and child choices. }
  TNyxTabsEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Inherit: Boolean;
    Options: TNyxTabsOptions;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTabsEditorChange; static;
  end;
  { Disposable form input is parked across panel switches. Exact owner/baseline
    prevents a draft from crossing projects, selection or inherited edits. No
    document, interface, renderer or node pointer survives in this value. }
  TNyxTabsEditorDraft = record
  private
    FEditor: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FValues: array[nteInitial..nteWrap] of TNyxText;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
  end;

{ Stable part identity; field enums never stand for arbitrary property methods. }
function NyxTabsEditorFieldID(const AEditor: TNyxText; AField: TNyxTabsEditorField): TNyxText;
{ Borrow the complete document for independent realization. Missing/non-tabs
  projection refuses; the returned exact baseline retains no source references. }
function NyxTabsEditorBaseline(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxText;
{ Public reusable Card/Select/Checkbox/Button parts, seeded from effective policy.
  Exact identities are stored separately from display choices; no source changes. }
function NewNyxTabsEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated buttons return False. Exact form shape and typed choice admission
  precede returning a copied proposal. The receiver rechecks Baseline and owner
  through its atomic paired command; Capture never changes a tree or history. }
function CaptureNyxTabsEditor(AButton, ARoot: TNyxNode; out AChange: TNyxTabsEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.composition, nyx.menu.editor;

const
  CEditor = 'nyx.tabs-editor';
  COwner = 'nyx.tabs-editor.owner';
  CBaseline = 'nyx.tabs-editor.baseline';
  CFields: array[TNyxTabsEditorField] of TNyxText =
    ('initial', 'activation', 'orientation', 'wrap', 'apply', 'inherit');
  CActivation: array[TNyxTabActivation] of TNyxText = ('On focus', 'On Enter or Space');
  COrientation: array[TNyxTabOrientation] of TNyxText = ('Horizontal', 'Vertical');
  CFirst = 'First available tab';

function NyxTabsEditorFieldID(const AEditor: TNyxText; AField: TNyxTabsEditorField): TNyxText;
begin
  Result := AEditor + TNyxText('-') + CFields[AField];
end;

function TNyxTabsEditorChange.ToData: TNyxDataValue;
var
  LOptions: TNyxDataValue;
begin
  NyxControl(Owner.ID);

  if Baseline = '' then
  begin
    raise ENyxModel.Create('Tab editor intent requires its exact baseline');
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

class function TNyxTabsEditorChange.FromData(const AData: TNyxDataValue): TNyxTabsEditorChange;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 4) then
  begin
    raise ENyxModel.Create('Tab editor intent requires four exact fields');
  end;
  Result := Default(TNyxTabsEditorChange);
  Result.Owner := NyxControl(AData.Field('owner').AsText);
  Result.Baseline := AData.Field('baseline').AsText;
  Result.Inherit := AData.Field('inherit').AsBoolean;
  Result.Options := TNyxTabsOptions.Defaults;

  if Result.Inherit then
  begin

    if AData.Field('options').Kind <> ndNull then
    begin
      raise ENyxModel.Create('Inherited tab policy cannot contain explicit options');
    end;
  end
  else
  begin
    Result.Options := TNyxTabsOptions.FromData(AData.Field('options'));
  end;
  Result.ToData;
end;

function NyxTabsEditorBaseline(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
  LProjection: TNyxNode;
  LContext: TNyxNode;
  LLocal: TNyxDataValue;
  LChildren: array of TNyxDataValue;
  LIndex: Integer;
  LState: TNyxTabState;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Tab editor requires its document');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('The tab editor owner is absent');
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LProjection);
  try
    LState := TNyxTabState.Create(LProjection);
    try
      SetLength(LChildren, LState.Count);
      for LIndex := 0 to High(LChildren) do
      begin
        LChildren[LIndex] := NyxObject([
          NyxField('id', NyxData(LProjection.Children[LIndex].SourceID)),
          NyxField('caption', NyxData(LState.Entry(LIndex).Caption)),
          NyxField('available', NyxData(LState.Entry(LIndex).Available))]);
      end;
      LLocal := NyxNull;

      if LOwner.Extensions.Has(NyxExtension(NyxTabsOptionsKey)) then
      begin
        LLocal := LOwner.Extensions.Value(NyxExtension(NyxTabsOptionsKey));
      end;
      Result := NyxObject([NyxField('local', LLocal),
        NyxField('effective', LState.Options.ToData),
        NyxField('children', NyxArray(LChildren))]).ToJSON;
    finally
      LState.Free;
    end;
  finally
    LContext.Free;
  end;
end;

function ChildChoice(const AChildren: TNyxDataValue; AIndex: Integer): TNyxText;
begin
  { The shared bounded Unicode choice formatter gives identical captions an
    unambiguous ordinal. The copied descriptor retains the separate exact ID. }
  Result := NyxMenuEditorChoiceCaption(AIndex, AChildren.Item(AIndex).Field('caption').AsText);
end;

function NewNyxTabsEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxDataValue;
  LOptions: TNyxTabsOptions;
  LChildren: TNyxDataValue;
  LItems: TNyxText;
  LInitial: TNyxText;
  LIndex: Integer;
  LButton: INyxButton;
  LField: TNyxTabsEditorField;
begin
  LBaseline := TNyxDataValue.ParseJSON(NyxTabsEditorBaseline(ADocument, AOwner));
  LOptions := TNyxTabsOptions.FromData(LBaseline.Field('effective'));
  LChildren := LBaseline.Field('children');
  LItems := CFirst;
  LInitial := CFirst;
  for LIndex := 0 to LChildren.Count - 1 do
  begin
    LItems := LItems + #10 + ChildChoice(LChildren, LIndex);

    if LChildren.Item(LIndex).Field('id').AsText = LOptions.InitialTab.ID then
    begin
      LInitial := ChildChoice(LChildren, LIndex);
    end;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Gap(10).Padding(16).Done;
  Result.Node.SetProp(COwner, AOwner.ID).SetProp(CBaseline, LBaseline.ToJSON);
  Result.Add(NewNyxLabel(AID + TNyxText('-title')).Configure.PartName(NyxPart('title')).Text('Tab behavior').Done);
  Result.Add(NewNyxSelect(NyxTabsEditorFieldID(AID, nteInitial)).Configure
    .PartName(NyxPart(CFields[nteInitial])).Text('Initial tab').Items(LItems).Value(LInitial).Done);
  Result.Add(NewNyxSelect(NyxTabsEditorFieldID(AID, nteActivation)).Configure
    .PartName(NyxPart(CFields[nteActivation])).Text('Activate tab')
    .Items(CActivation[ntaAutomatic] + #10 + CActivation[ntaManual])
    .Value(CActivation[LOptions.ActivationMode]).Done);
  Result.Add(NewNyxSelect(NyxTabsEditorFieldID(AID, nteOrientation)).Configure
    .PartName(NyxPart(CFields[nteOrientation])).Text('Header direction')
    .Items(COrientation[ntoHorizontal] + #10 + COrientation[ntoVertical])
    .Value(COrientation[LOptions.OrientationMode]).Done);
  Result.Add(NewNyxCheckbox(NyxTabsEditorFieldID(AID, nteWrap)).Configure
    .PartName(NyxPart(CFields[nteWrap])).Text('Wrap at the first and last tab').Value(LOptions.WrapFocus).Done);
  for LField := nteApply to nteInherit do
  begin
    LButton := NewNyxButton(NyxTabsEditorFieldID(AID, LField));
    LButton.Configure.PartName(NyxPart(CFields[LField])).Text('Apply tab behavior').Done;

    if LField = nteInherit then
    begin
      LButton.Configure.Text('Use inherited behavior').Enabled(LBaseline.Field('local').Kind <> ndNull).Done;
    end;
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
end;

function FormField(AEditor: TNyxNode; AField: TNyxTabsEditorField): TNyxNode;
var
  LKind: TNyxKind;
begin
  Result := AEditor.Part(NyxPart(CFields[AField]));
  LKind := nkSelect;

  if AField = nteWrap then
  begin
    LKind := nkCheckbox;
  end;

  if (Result = nil) or (Result.ProjectionKind <> NyxKindName(LKind)) then
  begin
    raise ENyxModel.Create('The mounted tab form is incomplete');
  end;
end;

function CaptureNyxTabsEditor(AButton, ARoot: TNyxNode; out AChange: TNyxTabsEditorChange): Boolean;
var
  LID: TNyxText;
  LEditor: TNyxNode;
  LBaseline: TNyxDataValue;
  LChildren: TNyxDataValue;
  LChoice: TNyxText;
  LIndex: Integer;
  LFound: Boolean;
begin
  AChange := Default(TNyxTabsEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditor);

  if (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('This tab form belongs to a retired view');
  end;
  LEditor := AButton.Parent;
  while (LEditor <> nil) and (LEditor.SourceID <> LID) do
  begin
    LEditor := LEditor.Parent;
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxModel.Create('Select this tab form again before applying');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwner));
  AChange.Baseline := LEditor.Prop(CBaseline);
  LBaseline := TNyxDataValue.ParseJSON(AChange.Baseline);
  LChildren := LBaseline.Field('children');
  AChange.Inherit := AButton.SourceID = NyxTabsEditorFieldID(LID, nteInherit);

  if not AChange.Inherit and (AButton.SourceID <> NyxTabsEditorFieldID(LID, nteApply)) then
  begin
    raise ENyxModel.Create('Unknown tab form action');
  end;
  AChange.Options := TNyxTabsOptions.Defaults;

  if AChange.Inherit then
  begin

    if LBaseline.Field('local').Kind = ndNull then
    begin
      raise ENyxModel.Create('There is no local tab declaration to inherit');
    end;
    Exit(True);
  end;
  LChoice := FormField(LEditor, nteInitial).Prop('value');
  LFound := LChoice = CFirst;
  for LIndex := 0 to LChildren.Count - 1 do
  begin

    if LChoice = ChildChoice(LChildren, LIndex) then
    begin

      if not LChildren.Item(LIndex).Field('available').AsBoolean then
      begin
        raise ENyxModel.Create('Choose an available initial tab');
      end;
      AChange.Options := AChange.Options.Initial(NyxControl(LChildren.Item(LIndex).Field('id').AsText));
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Unknown initial tab choice');
  end;
  LChoice := FormField(LEditor, nteActivation).Prop('value');

  if LChoice = CActivation[ntaManual] then
  begin
    AChange.Options := AChange.Options.Activation(ntaManual);
  end
  else if LChoice <> CActivation[ntaAutomatic] then
  begin
    raise ENyxModel.Create('Unknown tab activation choice');
  end;
  LChoice := FormField(LEditor, nteOrientation).Prop('value');

  if LChoice = COrientation[ntoVertical] then
  begin
    AChange.Options := AChange.Options.Orientation(ntoVertical);
  end
  else if LChoice <> COrientation[ntoHorizontal] then
  begin
    raise ENyxModel.Create('Unknown tab orientation choice');
  end;
  LChoice := FormField(LEditor, nteWrap).Prop('value');

  if (LChoice <> 'true') and (LChoice <> 'false') then
  begin
    raise ENyxModel.Create('Tab wrapping requires a Boolean field');
  end;
  AChange.Options := AChange.Options.Wrap(LChoice = 'true');
  AChange.ToData;
  Result := True;
end;

procedure TNyxTabsEditorDraft.Clear;
var
  LField: TNyxTabsEditorField;
begin
  FEditor := '';
  FOwner := '';
  FBaseline := '';
  for LField := nteInitial to nteWrap do
  begin
    FValues[LField] := '';
  end;
end;

procedure TNyxTabsEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LField: TNyxTabsEditorField;
  LNext: TNyxTabsEditorDraft;
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
  LNext := Default(TNyxTabsEditorDraft);
  LNext.FEditor := AEditor;
  LNext.FOwner := LEditor.Prop(COwner);
  LNext.FBaseline := LEditor.Prop(CBaseline);
  for LField := nteInitial to nteWrap do
  begin
    LNext.FValues[LField] := FormField(LEditor, LField).Prop('value');
  end;
  Self := LNext;
end;

function TNyxTabsEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxTabsEditorField;
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
  { Validate all four fields before writing any value into the disposable form. }
  for LField := nteInitial to nteWrap do
  begin
    FormField(LEditor, LField);
  end;
  for LField := nteInitial to nteWrap do
  begin
    FormField(LEditor, LField).SetProp('value', FValues[LField]);
  end;
  Result := True;
end;

end.
