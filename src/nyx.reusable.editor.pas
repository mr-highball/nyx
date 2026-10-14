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

unit nyx.reusable.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data;

type
  TNyxReusableIdentities = array of TNyxIdentityAssignment;
  TNyxReusableEditorField = (nrfName, nrfPreview, nrfCreate, nrfSummary);
  { Copied form state contains no tree/renderer. An absent form parks its input;
    changed source subtree or occupied identity set retires it before restoration. }
  TNyxReusableEditorDraft = record
  private
    FEditor: TNyxText;
    FSource: TNyxText;
    FBaseline: TNyxText;
    FName: TNyxText;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
  end;
  { Independently copied intent. The source remains authored; the new definition
    owns a copy and every descendant has an explicit purpose-based destination.
    Mapping arrays returned to callers are detached on native and browser. }
  TNyxReusableProposal = record
  private
    FSource: TNyxControlRef;
    FDefinition: TNyxComponentRef;
    FBaseline: TNyxText;
    FIdentities: TNyxReusableIdentities;
    function GetIdentities: TNyxReusableIdentities;
  public
    property Source: TNyxControlRef read FSource;
    property Definition: TNyxComponentRef read FDefinition;
    property Baseline: TNyxText read FBaseline;
    property Identities: TNyxReusableIdentities read GetIdentities;
  end;

{ Stable names/parts use closed enums; invalid enum casts refuse. }
function NyxReusableEditorFieldID(const AID: TNyxText;
  AField: TNyxReusableEditorField): TNyxText;
{ Snapshot exactly this authored subtree and all occupied design identities.
  Defaults/resources remain under the owning document's ordinary paired admission.
  Nil/missing sources refuse; no supplied object is retained or mutated. }
function NyxReusableEditorBaseline(ADocument: TNyxDocument;
  const ASource: TNyxControlRef): TNyxText;
{ Validate a complete detached clone before returning its identity plan. Exact
  definition names are not normalized; occupied names refuse. Descendant stems
  preserve source purpose with a bounded component prefix and collision suffix.
  IDs retain scalar boundaries; source metadata, slots/bindings remain untouched. }
function NyxReusableProposal(ADocument: TNyxDocument; const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef): TNyxReusableProposal;
{ Public managed Card/Input/Button composition with named reusable parts.
  Preview and Create have distinct stable identities; transfer Card ownership
  normally. The source/document are borrowed only during construction. }
function NewNyxReusableEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const ASource: TNyxControlRef): INyxCard;
{ Unrelated controls return False. Exact form fields/source/baseline must match;
  returns copied input only, with no source mutation or history side effect. }
function ReadNyxReusableEditor(AButton, ARoot: TNyxNode;
  out ASource: TNyxControlRef; out ADefinition: TNyxComponentRef;
  out ABaseline: TNyxText; out APreview: Boolean): Boolean;
{ Show at most eight copied destinations and the total. This is presentation
  only; caller synchronizes its mounted adapter after the label change. }
procedure ShowNyxReusablePreview(const AID: TNyxText; ARoot: TNyxNode;
  const AProposal: TNyxReusableProposal);

implementation

uses
  nyx.codec, nyx.composition, nyx.text.index;

type
  TNyxReusableDataValues = array of TNyxDataValue;

const
  CSource = 'nyx.reusable-editor.source';
  CBaseline = 'nyx.reusable-editor.baseline';
  CEditor = 'nyx.reusable-editor';
  CFields: array[TNyxReusableEditorField] of TNyxText =
    ('name', 'preview', 'create', 'summary');

function PrefixScalars(const AText: TNyxText; ALimit: Integer): TNyxText;
var
  LIndex: Integer;
  LStart: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  LCount := 0;
  LStart := 1;
  while (LIndex <= Length(AText)) and (LCount < ALimit) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Reusable name contains malformed Unicode');
    end;
    LStart := LIndex;
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LStart - 1);
end;

function NyxReusableEditorFieldID(const AID: TNyxText;
  AField: TNyxReusableEditorField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxReusableEditorField))) or
    (Ord(AField) > Ord(High(TNyxReusableEditorField))) then
  begin
    raise ENyxModel.Create('Unknown reusable editor field');
  end;
  Result := AID + '-' + CFields[AField];
end;

procedure CollectIDs(ANode: TNyxNode; AIndex: TNyxTextIndex;
  var AValues: TNyxReusableDataValues);
var
  LChild: Integer;
begin

  if AIndex <> nil then
  begin
    AIndex.AddFirst(ANode.ID, Length(AValues));
  end;
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := NyxData(ANode.ID);
  for LChild := 0 to ANode.Count - 1 do
  begin
    CollectIDs(ANode.Children[LChild], AIndex, AValues);
  end;
end;

procedure DocumentIDs(ADocument: TNyxDocument; AIndex: TNyxTextIndex;
  var AValues: TNyxReusableDataValues);
var
  LRoot: Integer;
begin
  for LRoot := 0 to ADocument.Count - 1 do
  begin
    CollectIDs(ADocument.Pages[LRoot], AIndex, AValues);
  end;
  for LRoot := 0 to ADocument.ComponentCount - 1 do
  begin
    CollectIDs(ADocument.Components[LRoot], AIndex, AValues);
  end;
end;

function NyxReusableEditorBaseline(ADocument: TNyxDocument;
  const ASource: TNyxControlRef): TNyxText;
var
  LSource: TNyxNode;
  LIDs: TNyxReusableDataValues;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Reusable editor requires its owning document');
  end;
  LSource := ADocument.Find(ASource.ID);

  if LSource = nil then
  begin
    raise ENyxModel.Create('Reusable source is no longer in this document');
  end;
  DocumentIDs(ADocument, nil, LIDs);
  Result := NyxObject([NyxField('source', TNyxCodec.NodeSnapshot(LSource)),
    NyxField('occupied', NyxArray(LIDs))]).ToJSON;
end;

function TNyxReusableProposal.GetIdentities: TNyxReusableIdentities;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FIdentities));
  for LIndex := 0 to High(FIdentities) do
  begin
    Result[LIndex] := FIdentities[LIndex];
  end;
end;

function NyxReusableProposal(ADocument: TNyxDocument; const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef): TNyxReusableProposal;
var
  LSource: TNyxNode;
  LClone: TNyxNode;
  LOccupied: TNyxTextIndex;
  LIDs: TNyxReusableDataValues;
  LPrefix: TNyxText;
  LPlan: TNyxReusableProposal;

  procedure Identify(ANode: TNyxNode);
  var
    LChild: Integer;
    LCount: Integer;
    LSerial: Integer;
    LStem: TNyxText;
    LTarget: TNyxText;
  begin
    for LChild := 0 to ANode.Count - 1 do
    begin
      LStem := PrefixScalars(LPrefix + '-' + ANode.Children[LChild].ID, 116);
      LTarget := LStem;
      LSerial := 1;
      while LOccupied.IndexOf(LTarget) >= 0 do
      begin
        Inc(LSerial);

        if LSerial > NyxMaximumNodes + 2 then
        begin
          raise ENyxModel.Create('Reusable identity suffix budget exceeded');
        end;
        LTarget := LStem + '-' + TNyxText(IntToStr(LSerial));
      end;
      LOccupied.AddFirst(LTarget, 0);
      LCount := Length(LPlan.FIdentities);
      SetLength(LPlan.FIdentities, LCount + 1);
      LPlan.FIdentities[LCount] := NyxIdentity(NyxControl(ANode.Children[LChild].ID),
        NyxControl(LTarget));
      Identify(ANode.Children[LChild]);
    end;
  end;

begin
  LPlan := Default(TNyxReusableProposal);
  LPlan.FSource := ASource;
  LPlan.FDefinition := ADefinition;
  LPlan.FBaseline := NyxReusableEditorBaseline(ADocument, ASource);
  LSource := ADocument.Find(ASource.ID);
  LOccupied := TNyxTextIndex.Create;
  LClone := nil;
  try
    DocumentIDs(ADocument, LOccupied, LIDs);

    if LOccupied.IndexOf(ADefinition.Name) >= 0 then
    begin
      raise ENyxModel.Create('Reusable name is already occupied: ' + ADefinition.Name);
    end;
    LOccupied.AddFirst(ADefinition.Name, 0);
    LPrefix := PrefixScalars(ADefinition.Name, 32);
    Identify(LSource);
    LClone := CloneNyxReusableDefinition(ADocument, LSource, ADefinition, LPlan.FIdentities);
    Result := LPlan;
  finally
    ReleaseNyxNode(LClone);
    LOccupied.Free;
  end;
end;

function NewNyxReusableEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const ASource: TNyxControlRef): INyxCard;
var
  LName: TNyxText;
  LSerial: Integer;
  LBaseline: TNyxText;
  LField: TNyxReusableEditorField;
  LButton: INyxButton;
begin
  LBaseline := NyxReusableEditorBaseline(ADocument, ASource);
  LName := PrefixScalars(ASource.ID, 96) + '-component';
  LSerial := 1;
  while ADocument.Find(LName) <> nil do
  begin
    Inc(LSerial);
    LName := PrefixScalars(ASource.ID, 96) + '-component-' + TNyxText(IntToStr(LSerial));
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Gap(8).Padding(12).Done;
  Result.Node.SetProp(CSource, ASource.ID).SetProp(CBaseline, LBaseline);
  Result.Add(NewNyxLabel(AID + '-title').WithText('Create a reusable copy'));
  Result.Add(NewNyxInput(NyxReusableEditorFieldID(AID, nrfName)).Configure
    .PartName(NyxPart('name')).Text('Component name').Value(LName).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'The original stays in place. Preview the copied control names before creating the definition.'));
  for LField := nrfPreview to nrfCreate do
  begin
    LButton := NewNyxButton(NyxReusableEditorFieldID(AID, LField));
    LButton.Configure.PartName(NyxPart(CFields[LField])).Text('Preview names').Done;

    if LField = nrfCreate then
    begin
      LButton.Configure.Text('Create reusable copy').Variant(nvPrimary).Done;
    end;
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
  { Preformatted text preserves one mapping per line on both target adapters.
    The bounded face can scroll instead of compressing a large preview to prose. }
  Result.Add(NewNyxCode(NyxReusableEditorFieldID(AID, nrfSummary)).Configure
    .PartName(NyxPart('summary')).Height(180)
    .Text('Choose a name, then preview or create.').Done);
end;

function ReadNyxReusableEditor(AButton, ARoot: TNyxNode;
  out ASource: TNyxControlRef; out ADefinition: TNyxComponentRef;
  out ABaseline: TNyxText; out APreview: Boolean): Boolean;
var
  LEditor: TNyxNode;
  LName: TNyxNode;
  LID: TNyxText;
begin
  Result := False;
  ASource := Default(TNyxControlRef);
  ADefinition := Default(TNyxComponentRef);
  ABaseline := '';
  APreview := False;

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditor);

  if ARoot = nil then
  begin
    raise ENyxModel.Create('Reusable form requires its live owning tree');
  end;
  APreview := AButton.SourceID = NyxReusableEditorFieldID(LID, nrfPreview);

  if not APreview and (AButton.SourceID <> NyxReusableEditorFieldID(LID, nrfCreate)) then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(LID);

  if LEditor = nil then
  begin
    raise ENyxModel.Create('Reusable form is no longer mounted');
  end;

  if LEditor.Find(AButton.ID) <> AButton then
  begin
    raise ENyxModel.Create('Reusable action belongs to a different form');
  end;
  LName := LEditor.Find(NyxReusableEditorFieldID(LID, nrfName));

  if (LName = nil) or (LName.Kind <> NyxKindName(nkInput)) then
  begin
    raise ENyxModel.Create('Reusable form requires its typed name input');
  end;
  ASource := NyxControl(LEditor.Prop(CSource));
  ADefinition := NyxComponent(LName.Prop(NyxAttributeName(atValue)));
  ABaseline := LEditor.Prop(CBaseline);
  Result := True;
end;

procedure ShowNyxReusablePreview(const AID: TNyxText; ARoot: TNyxNode;
  const AProposal: TNyxReusableProposal);
var
  LSummary: TNyxNode;
  LText: TNyxText;
  LIndex: Integer;
begin
  LSummary := ARoot.Find(NyxReusableEditorFieldID(AID, nrfSummary));

  if LSummary = nil then
  begin
    raise ENyxModel.Create('Reusable preview summary is missing');
  end;
  LText := 'Definition: ' + AProposal.Definition.Name + #10 +
    TNyxText(IntToStr(Length(AProposal.FIdentities))) + ' copied descendants';
  for LIndex := 0 to High(AProposal.FIdentities) do
  begin

    if LIndex >= 8 then
    begin
      LText := LText + #10 + 'More names are included in the complete typed copy.';
      Break;
    end;
    LText := LText + #10 + AProposal.FIdentities[LIndex].Source.ID + TNyxText(' → ') +
      AProposal.FIdentities[LIndex].Destination.ID;
  end;
  LSummary.Configure.Text(LText).Done;
end;

procedure TNyxReusableEditorDraft.Clear;
begin
  Self := Default(TNyxReusableEditorDraft);
end;

procedure TNyxReusableEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LName: TNyxNode;
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
  LName := LEditor.Find(NyxReusableEditorFieldID(AEditor, nrfName));

  if (LName = nil) or (LName.Kind <> NyxKindName(nkInput)) then
  begin
    Clear;
    Exit;
  end;
  FEditor := AEditor;
  FSource := LEditor.Prop(CSource);
  FBaseline := LEditor.Prop(CBaseline);
  FName := LName.Prop(NyxAttributeName(atValue));
end;

function TNyxReusableEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LName: TNyxNode;
begin
  Result := False;

  if (ARoot = nil) or (FEditor = '') then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(FEditor);

  if LEditor = nil then
  begin
    Exit;
  end;
  LName := LEditor.Find(NyxReusableEditorFieldID(FEditor, nrfName));

  if (LEditor.Prop(CSource) <> FSource) or (LEditor.Prop(CBaseline) <> FBaseline) or
    (LName = nil) or (LName.Kind <> NyxKindName(nkInput)) then
  begin
    Clear;
    Exit;
  end;
  LName.Configure.Value(FName).Done;
  Result := True;
end;

end.
