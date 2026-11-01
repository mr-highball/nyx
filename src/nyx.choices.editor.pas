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
unit nyx.choices.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.choices, nyx.menu.editor;

type
  { Closed ordinary form fields. The selected ordinal is disposable navigation,
    never the application value or an identity used to publish a selection. }
  TNyxChoiceEditorField = (ncefSelected, ncefKey, ncefEscaped, ncefCaption);
  { Each explicit action proposes one complete set or restores inheritance. }
  TNyxChoiceEditorAction = (nceaSave, nceaAdd, nceaUp, nceaDown, nceaRemove,
    nceaEmpty, nceaInherit);
  { Copied proposal retains immutable admitted choice data, exact owner/baseline.
    Interface getters reconstruct owned snapshots; pas2js record storage never
    contains a COM-interface or retains an external provider.
    Inherit has no Items; an empty Items is an explicit local mask. The host must
    recheck baseline and publish through its own atomic history/source command. }
  TNyxChoiceEditorChange = record
  private
    FItems: TNyxDataValue;
    function GetItems: INyxChoiceItems;
    procedure SetItems(const AItems: INyxChoiceItems);
  public
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Inherit: Boolean;
    property Items: INyxChoiceItems read GetItems write SetItems;
  end;
  { Owned scalar form snapshot. Incomplete input survives panel switches only
    for this same exact local/effective baseline; no model/widget is retained. }
  TNyxChoiceEditorDraft = record
  private
    FEditor: TNyxText;
    FFields: TNyxMenuEditorDraft;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
  end;

function NyxChoiceEditorFieldID(const AEditor: TNyxText;
  AField: TNyxChoiceEditorField): TNyxText;
function NyxChoiceEditorActionID(const AEditor: TNyxText;
  AAction: TNyxChoiceEditorAction): TNyxText;
{ Independent realization checks effective select applicability, including
  reusable roots/parts. Returned context is text, not an ownership lease. }
function NyxChoiceEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ Public Nyx form edits one choice at a time, keeping all other exact keys/order.
  Exceptional control-character keys use an explicit escaped-key mode. Captions
  in the navigation selector are bounded; exact authored captions stay in fields.
  The borrowed document is never changed during construction or capture. }
function NewNyxChoiceEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
{ Presentation-only row navigation. The caller refreshes/syncs its form and
  captures drafts normally; no document mutation, source or history occurs. }
function RouteNyxChoiceEditorSelection(ANode, ARoot: TNyxNode): Boolean;
{ Unrelated input returns False. Current membership, exact complete context and
  typed admission precede a proposal; malformed input leaves every field intact. }
function CaptureNyxChoiceEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxChoiceEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.composition, nyx.select.configuration;

const
  CEditor = 'nyx.choices-editor';
  CAction = 'nyx.choices-editor.action';
  COwner = NyxMenuFormOwnerKey;
  CBaseline = NyxMenuFormBaselineKey;
  CFields: array[TNyxChoiceEditorField] of TNyxText =
    ('selected', 'key', 'escaped', 'caption');
  CActions: array[TNyxChoiceEditorAction] of TNyxText =
    ('save', 'add', 'up', 'down', 'remove', 'empty', 'inherit');
  CLabels: array[TNyxChoiceEditorAction] of TNyxText =
    ('Save choice', 'Add choice', 'Move choice up', 'Move choice down',
      'Remove choice', 'No choices', 'Use inherited choices');

function TNyxChoiceEditorChange.GetItems: INyxChoiceItems;
begin
  Result := nil;

  if FItems.Kind <> ndNull then
  begin
    Result := NyxChoicesFromData(FItems);
  end;
end;

procedure TNyxChoiceEditorChange.SetItems(const AItems: INyxChoiceItems);
var
  LItems: INyxChoiceItems;
  LCandidate: TNyxDataValue;
begin
  LItems := AItems;
  LCandidate := NyxNull;

  if LItems <> nil then
  begin
    LCandidate := NyxChoicesFromData(LItems.ToData).ToData;
  end;
  FItems := LCandidate;
end;

function NyxChoiceEditorFieldID(const AEditor: TNyxText;
  AField: TNyxChoiceEditorField): TNyxText;
begin
  Result := AEditor + TNyxText('-') + CFields[AField];
end;

function NyxChoiceEditorActionID(const AEditor: TNyxText;
  AAction: TNyxChoiceEditorAction): TNyxText;
begin
  Result := AEditor + TNyxText('-') + CActions[AAction];
end;

function NyxChoiceEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LOwner: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxDataValue;
  LEffective: TNyxDataValue;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Choice editing requires its document');
  end;
  LOwner := ADocument.Find(NyxControl(AOwner.ID).ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Choice editor owner is absent');
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LProjection);
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkSelect)) then
    begin
      raise ENyxModel.Create('Choice editing requires an effective select');
    end;
    LLocal := NyxNull;
    LEffective := NyxNull;

    if LOwner.HasChoiceItems then
    begin
      LLocal := LOwner.ChoiceItems.ToData;
    end;

    if LProjection.HasChoiceItems then
    begin
      LEffective := LProjection.ChoiceItems.ToData;
    end;
    Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
      NyxField('local', LLocal), NyxField('effective', LEffective),
      NyxField('legacy', NyxData(LProjection.Prop(NyxAttributeName(atItems))))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

function FormItems(AForm: TNyxNode): INyxChoiceItems;
var
  LData: TNyxDataValue;
begin
  LData := TNyxDataValue.ParseJSON(AForm.Prop(CBaseline)).Field('effective');
  Result := NewNyxChoices;

  if LData.Kind <> ndNull then
  begin
    Result := NyxChoicesFromData(LData);
  end;
end;

function Field(AForm: TNyxNode; AField: TNyxChoiceEditorField): TNyxNode;
begin
  Result := AForm.Find(NyxChoiceEditorFieldID(AForm.SourceID, AField));

  if Result = nil then
  begin
    raise ENyxModel.Create('Choice form is incomplete');
  end;
end;

function FormFor(ANode, ARoot: TNyxNode): TNyxNode;
var
  LID: TNyxText;
begin
  LID := ANode.Prop(CEditor);

  if (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxModel.Create('Choice form belongs to a retired view');
  end;
  Result := ANode.Parent;
  while (Result <> nil) and (Result.SourceID <> LID) do
  begin
    Result := Result.Parent;
  end;

  if (Result = nil) or (Result.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxModel.Create('Choice action requires its current mounted form');
  end;
end;

function SelectedIndex(AForm: TNyxNode; const AItems: INyxChoiceItems): Integer;
var
  LValue: TNyxText;
begin
  LValue := Field(AForm, ncefSelected).Prop('value');
  Result := -1;

  if not TryStrToInt(String(LValue), Result) or (Result < -1) or
    (Result >= AItems.Count) or (LValue <> TNyxText(IntToStr(Result))) then
  begin
    raise ENyxModel.Create('Choose an existing row or New choice');
  end;
end;

procedure RefreshActions(AForm: TNyxNode);
var
  LItems: INyxChoiceItems;
  LIndex: Integer;
  LAction: TNyxChoiceEditorAction;
  LEnabled: Boolean;
begin
  LItems := FormItems(AForm);
  LIndex := SelectedIndex(AForm, LItems);
  for LAction := Low(TNyxChoiceEditorAction) to High(TNyxChoiceEditorAction) do
  begin
    LEnabled := LIndex >= 0;
    case LAction of
      nceaSave, nceaRemove: LEnabled := LIndex >= 0;
      nceaAdd: LEnabled := LItems.Count < NyxMaximumChoiceItems;
      nceaUp: LEnabled := LIndex > 0;
      nceaDown: LEnabled := (LIndex >= 0) and (LIndex < LItems.Count - 1);
      nceaEmpty: LEnabled := True;
      nceaInherit: LEnabled := TNyxDataValue.ParseJSON(AForm.Prop(CBaseline))
        .Field('local').Kind <> ndNull;
    end;
    AForm.Find(NyxChoiceEditorActionID(AForm.SourceID, LAction))
      .Configure.Enabled(LEnabled).Done;
  end;
end;

procedure LoadSelection(AForm: TNyxNode);
var
  LItems: INyxChoiceItems;
  LIndex: Integer;
  LKey: TNyxText;
  LCaption: TNyxText;
  LEscaped: Boolean;
  LAt: Integer;
  LScalar: Integer;
begin
  LItems := FormItems(AForm);
  LIndex := SelectedIndex(AForm, LItems);
  LKey := '';
  LCaption := '';
  LEscaped := False;

  if LIndex >= 0 then
  begin
    LKey := LItems.Item(LIndex).Key.Value;
    LCaption := LItems.Item(LIndex).Caption;
    LAt := 1;
    while LAt <= Length(LKey) do
    begin

      if not NyxNextScalar(LKey, LAt, LScalar) then
      begin
        raise ENyxModel.Create('Choice key contains malformed Unicode');
      end;

      if (LScalar < 32) or ((LScalar >= $7f) and (LScalar <= $9f)) or
        (LScalar = $2028) or (LScalar = $2029) then
      begin
        LEscaped := True;
      end;
    end;

    if LEscaped then
    begin
      LKey := NyxData(LKey).ToJSON;
    end;
  end;
  Field(AForm, ncefKey).Configure.Value(LKey).Done;
  Field(AForm, ncefEscaped).Configure.Value(LEscaped).Done;
  Field(AForm, ncefCaption).Configure.Value(LCaption).Done;
  RefreshActions(AForm);
end;

function NewNyxChoiceEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
var
  LBaseline: TNyxText;
  LItems: INyxChoiceItems;
  LNavigation: INyxChoiceItems;
  LSelect: INyxSelect;
  LButton: INyxButton;
  LAction: TNyxChoiceEditorAction;
  LIndex: Integer;
  LNavigationRows: array of TNyxChoiceItem;
  LFirst: Integer;
begin
  LBaseline := NyxChoiceEditorBaseline(ADocument, AOwner);
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Extension(COwner, AOwner.ID)
    .Extension(CBaseline, LBaseline).Done;
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).WithText('Choices'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Keys are stored values. Captions are shown to people. Save applies one undoable change.'));
  LItems := FormItems(Result.Node);
  LFirst := Ord(LItems.Count < NyxMaximumChoiceItems);
  SetLength(LNavigationRows, LItems.Count + LFirst);

  if LFirst > 0 then
  begin
    LNavigationRows[0] := NyxChoiceItem(NyxChoice('-1'), 'New choice');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    { Bounded navigation captions cannot make a maximum-size authored set
      exceed the selector's own text budget. Exact captions remain in the form. }
    LNavigationRows[LIndex + LFirst] := NyxChoiceItem(NyxChoice(TNyxText(IntToStr(LIndex))),
      NyxMenuEditorChoiceCaption(LIndex, LItems.Item(LIndex).Caption));
  end;
  LNavigation := NewNyxChoices(LNavigationRows);
  LSelect := NewNyxSelect(NyxChoiceEditorFieldID(AID, ncefSelected));
  LSelect.Configure.Text('Choice to edit').Value('-1').Extension(CEditor, AID).Done;

  if LFirst = 0 then
  begin
    LSelect.Configure.Value('0').Done;
  end;
  NyxSelectConfigure(LSelect).Choices(LNavigation);
  Result.Add(LSelect);
  Result.Add(NewNyxInput(NyxChoiceEditorFieldID(AID, ncefKey))
    .Configure.Text('Stable key').Value('').Done);
  Result.Add(NewNyxCheckbox(NyxChoiceEditorFieldID(AID, ncefEscaped))
    .Configure.Text('Escaped key (quoted JSON string for special characters)').Value(False).Done);
  Result.Add(NewNyxInput(NyxChoiceEditorFieldID(AID, ncefCaption))
    .Configure.Text('Display caption').Value('').Done);
  for LAction := Low(TNyxChoiceEditorAction) to High(TNyxChoiceEditorAction) do
  begin
    LButton := NewNyxButton(NyxChoiceEditorActionID(AID, LAction));
    LButton.Configure.Text(CLabels[LAction]).AccessibleName(CLabels[LAction])
      .Extension(CEditor, AID).Extension(CAction, CActions[LAction]).Done;

    if LAction = nceaInherit then
    begin
      LButton.Configure.Enabled(TNyxDataValue.ParseJSON(LBaseline)
        .Field('local').Kind <> ndNull).Done;
    end;
    Result.Add(LButton);
  end;
  LoadSelection(Result.Node);
end;

function RouteNyxChoiceEditorSelection(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
begin
  Result := (ANode <> nil) and (ANode.Prop(CEditor) <> '') and
    (ANode.SourceID = NyxChoiceEditorFieldID(ANode.Prop(CEditor), ncefSelected));

  if Result then
  begin
    LForm := FormFor(ANode, ARoot);
    LoadSelection(LForm);
  end;
end;

function CaptureNyxChoiceEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxChoiceEditorChange): Boolean;
var
  LForm: TNyxNode;
  LItems: INyxChoiceItems;
  LAction: TNyxChoiceEditorAction;
  LIndex: Integer;
  LAt: Integer;
  LFrom: Integer;
  LSwap: Integer;
  LKey: TNyxText;
  LEscaped: TNyxText;
  LCaption: TNyxText;
  LRows: array of TNyxChoiceItem;
begin
  AChange := Default(TNyxChoiceEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.Prop(CAction) = '') then
  begin
    Exit;
  end;
  LForm := FormFor(AButton, ARoot);
  AChange.Owner := NyxControl(LForm.Prop(COwner));
  AChange.Baseline := LForm.Prop(CBaseline);

  if AChange.Baseline <> NyxChoiceEditorBaseline(ADocument, AChange.Owner) then
  begin
    raise ENyxModel.Create('Choices changed since this form was opened');
  end;
  LAction := Low(TNyxChoiceEditorAction);
  while (CActions[LAction] <> AButton.Prop(CAction)) and
    (LAction < High(TNyxChoiceEditorAction)) do
  begin
    LAction := TNyxChoiceEditorAction(Ord(LAction) + 1);
  end;

  if (CActions[LAction] <> AButton.Prop(CAction)) or
    (AButton.SourceID <> NyxChoiceEditorActionID(LForm.SourceID, LAction)) then
  begin
    raise ENyxModel.Create('Unknown choice form action');
  end;
  AChange.Inherit := LAction = nceaInherit;

  if AChange.Inherit then
  begin

    if TNyxDataValue.ParseJSON(AChange.Baseline).Field('local').Kind = ndNull then
    begin
      raise ENyxModel.Create('There is no local choice declaration to inherit');
    end;
    Exit(True);
  end;
  AChange.Items := NewNyxChoices;

  if LAction = nceaEmpty then
  begin
    Exit(True);
  end;
  LItems := FormItems(LForm);
  LIndex := SelectedIndex(LForm, LItems);

  if (LAction <> nceaAdd) and (LIndex < 0) then
  begin
    raise ENyxModel.Create('Choose an existing choice before changing it');
  end;
  LKey := '';
  LCaption := '';

  if LAction in [nceaSave, nceaAdd] then
  begin
    LKey := Field(LForm, ncefKey).Prop('value');
    LEscaped := Field(LForm, ncefEscaped).Prop('value');

    if (LEscaped <> 'true') and (LEscaped <> 'false') then
    begin
      raise ENyxModel.Create('Escaped key mode requires a Boolean');
    end;

    if LEscaped = 'true' then
    begin
      LKey := TNyxDataValue.ParseJSON(LKey).AsText;
    end;
    LCaption := Field(LForm, ncefCaption).Prop('value');
  end;
  SetLength(LRows, LItems.Count + Ord(LAction = nceaAdd) - Ord(LAction = nceaRemove));
  LSwap := LIndex;

  if LAction = nceaUp then
  begin
    Dec(LSwap);
  end;

  if LAction = nceaDown then
  begin
    Inc(LSwap);
  end;

  if (LAction in [nceaUp, nceaDown]) and ((LSwap < 0) or (LSwap >= LItems.Count)) then
  begin
    raise ENyxModel.Create('This choice is already at that boundary');
  end;
  for LAt := 0 to High(LRows) do
  begin
    LFrom := LAt;

    if ((LAction = nceaAdd) and (LAt = LItems.Count)) or
      ((LAction = nceaSave) and (LAt = LIndex)) then
    begin
      LRows[LAt] := NyxChoiceItem(NyxChoice(LKey), LCaption);
      Continue;
    end;

    if (LAction = nceaRemove) and (LAt >= LIndex) then
    begin
      Inc(LFrom);
    end;

    if LAction in [nceaUp, nceaDown] then
    begin

      if LAt = LIndex then
      begin
        LFrom := LSwap;
      end
      else if LAt = LSwap then
      begin
        LFrom := LIndex;
      end;
    end;
    LRows[LAt] := LItems.Item(LFrom);
  end;
  AChange.Items := NewNyxChoices(LRows);
  Result := True;
end;

procedure TNyxChoiceEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
begin
  LEditor := nil;

  if ARoot <> nil then
  begin
    LEditor := ARoot.Find(AEditor);
  end;

  if (LEditor <> nil) and (LEditor.Prop(COwner) <> '') and
    (LEditor.Prop(CBaseline) <> '') then
  begin
    { Match the shared draft's admission: an absent editor parks its earlier
      identity as well as its fields, even if another form name was requested. }
    FEditor := AEditor;
  end;
  FFields.Capture(AEditor, ARoot);
end;

function TNyxChoiceEditorDraft.Restore(ARoot: TNyxNode): Boolean;
begin
  Result := FFields.Restore(ARoot);

  if Result then
  begin
    { Restore navigation availability without replacing the unfinished fields
      with their accepted row values. Those inputs remain wholly disposable. }
    RefreshActions(ARoot.Find(FEditor));
  end;
end;

procedure TNyxChoiceEditorDraft.Clear;
begin
  FEditor := '';
  FFields.Clear;
end;

end.
