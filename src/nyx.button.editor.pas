{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.button.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.button.presentation;

type
  { Ordinary form roles. Behavior is decoded into closed presentation enums;
    only image locations and resource identities remain open caller data. }
  TNyxButtonEditorField = (befImage, befResource, befContent, befDensity,
    befAlignment, befWeight, befTone, befSize, befGap, befColumns, befRows,
    befColumn, befRow);
  { Unsubmitted values survive ordinary shell refreshes. Exact local/effective
    baseline ownership prevents a parked form overwriting a later selection. }
  TNyxButtonEditorDraft = record
  private
    FBaseline: TNyxText;
    FValues: array[TNyxButtonEditorField] of TNyxText;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    procedure Clear;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxButtonEditorDraft; static;
    property Defined: Boolean read GetDefined;
  end;

function NyxButtonEditorFieldID(const AID: TNyxText;
  AField: TNyxButtonEditorField): TNyxText;
function NyxButtonEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
function NewNyxButtonEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxColumn;
{ Capture only this complete currently mounted Apply button. The caller checks
  copied Owner/Baseline against its current selection before queuing one ordinary
  atomic property command. A refused form changes neither design nor history. }
function CaptureNyxButtonEditor(AButton, ARoot: TNyxNode;
  out AOwner, ABaseline: TNyxText; out AValue: TNyxButtonPresentation): Boolean;

implementation

uses SysUtils, nyx.images, nyx.resources, nyx.resource.context, nyx.button.view;

const
  CMarker = 'nyx.button-editor';
  CMarkerKey = 'button-editor';
  COwnerKey = 'button-editor-owner';
  CBaselineKey = 'button-editor-baseline';
  CValueKey = 'button-editor-value';
  CKeys: array[TNyxButtonEditorField] of TNyxText =
    ('image', 'resource', 'content', 'density', 'alignment', 'weight', 'tone',
      'size', 'gap', 'columns', 'rows', 'column', 'row');
  CTitles: array[TNyxButtonEditorField] of TNyxText =
    ('Image source (URL or data URL)', 'Project image resource', 'Content', 'Density',
      'Alignment', 'Font weight', 'Glyph ink', 'Glyph size (px)', 'Glyph gap (px)',
      'Atlas columns', 'Atlas rows', 'Cell column (from 0)', 'Cell row (from 0)');
  CContent: array[TNyxButtonContent] of TNyxText =
    ('Caption', 'Glyph and caption', 'Glyph only');
  CDensity: array[TNyxButtonDensity] of TNyxText = ('Normal', 'Compact', 'Touch');
  CAlignment: array[TNyxButtonContentAlignment] of TNyxText = ('Centered', 'Leading');
  CWeight: array[TNyxButtonFontWeight] of TNyxText = ('Inherit', 'Regular', 'Semibold');
  CTone: array[TNyxGlyphTone] of TNyxText = ('Original colors', 'Theme foreground');

function Choice(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := Low(AChoices) to High(AChoices) do
  begin

    if AValue = AChoices[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxModel.Create('Select one of the available button style choices');
end;

function NyxButtonEditorFieldID(const AID: TNyxText;
  AField: TNyxButtonEditorField): TNyxText;
begin
  Result := AID + '-' + CKeys[AField];
end;

function Complete(AEditor: TNyxNode): Boolean;
var
  LField: TNyxButtonEditorField;
  LControl: TNyxNode;
  LKind: TNyxKind;
begin
  Result := (AEditor <> nil) and (AEditor.Kind = NyxKindName(nkColumn)) and
    (AEditor.ProjectionKind = NyxKindName(nkColumn)) and
    (AEditor.Prop(CMarkerKey) = CMarker) and (AEditor.Prop(COwnerKey) <> '') and
    (AEditor.Prop(CBaselineKey) <> '');

  if not Result then
  begin
    Exit;
  end;
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    LControl := AEditor.Find(NyxButtonEditorFieldID(AEditor.ID, LField));
    LKind := nkSelect;
    case LField of
      befImage, befResource, befSize, befGap, befColumns, befRows, befColumn, befRow: LKind := nkInput;
      befContent, befDensity, befAlignment, befWeight, befTone: ;
    end;

    if (LControl = nil) or (LControl.Parent <> AEditor) or
      (LControl.Kind <> NyxKindName(LKind)) or
      (LControl.ProjectionKind <> NyxKindName(LKind)) then
    begin
      Exit(False);
    end;
  end;
  LControl := AEditor.Find(AEditor.ID + '-apply');
  Result := (LControl <> nil) and (LControl.Parent = AEditor) and
    (LControl.Kind = NyxKindName(nkButton)) and
    (LControl.ProjectionKind = NyxKindName(nkButton));
end;

function NyxButtonEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
var
  LContext: INyxResourceContext;
  LResources: TNyxDataValue;
  LLocale, LFallback: TNyxText;
begin

  if (AOwner = nil) or (AProjection = nil) then
  begin
    raise ENyxModel.Create('Button editor requires its authored owner and effective projection');
  end;
  LResources := NyxNull;
  LLocale := '';
  LFallback := '';

  if AProjection.IsRealized then
  begin
    { Only the selected form captures this bounded catalog, never every button
      during paint. Exact catalog/locale values guard a parked resource choice
      against replacement while its unsubmitted fields are retained. }
    LContext := AProjection.ResourceContext;
    LResources := LContext.Snapshot.ToData;
    LLocale := LContext.Locale.Name;
    LFallback := LContext.Fallback.Name;
  end;
  Result := NyxObject([
    NyxField('owner', NyxData(AOwner.ID)),
    NyxField('kind', NyxData(AProjection.ProjectionKind)),
    NyxField('caption', NyxData(AProjection.Prop('text'))),
    NyxField('accessibleName', NyxData(AProjection.Prop('aria-label'))),
    NyxField('resources', LResources), NyxField('locale', NyxData(LLocale)),
    NyxField('fallback', NyxData(LFallback)),
    NyxField('localPresent', NyxData(AOwner.Props.IndexOfName(NyxAttributeName(atButtonPresentation)) >= 0)),
    NyxField('local', NyxData(AOwner.Prop(NyxAttributeName(atButtonPresentation)))),
    NyxField('effective', NyxButtonPresentationOf(AProjection).ToData)]).ToJSON;
end;

function NewNyxButtonEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxColumn;
var
  LValue: TNyxButtonPresentation;
  LValues: array[TNyxButtonEditorField] of TNyxText;
  LField: TNyxButtonEditorField;
  LControl: INyxControl;
  LChoices: TNyxText;
begin
  LValue := NyxButtonPresentationOf(AProjection);
  LValues[befImage] := '';
  LValues[befResource] := '';
  case LValue.GlyphKind of
    nbgNone: ; { Both source fields remain empty. }
    nbgImage: LValues[befImage] := LValue.ImageSource.ToWire;
    nbgResource: LValues[befResource] := LValue.ResourceImage.Reference.Name;
  end;
  LValues[befContent] := CContent[LValue.ContentMode];
  LValues[befDensity] := CDensity[LValue.ControlDensity];
  LValues[befAlignment] := CAlignment[LValue.ContentAlignment];
  LValues[befWeight] := CWeight[LValue.Weight];
  LValues[befTone] := CTone[LValue.Tone];
  LValues[befSize] := IntToStr(LValue.GlyphBoxSize);
  LValues[befGap] := IntToStr(LValue.GlyphSpacing);
  LValues[befColumns] := IntToStr(LValue.GlyphCell.Columns);
  LValues[befRows] := IntToStr(LValue.GlyphCell.Rows);
  LValues[befColumn] := IntToStr(LValue.GlyphCell.Column);
  LValues[befRow] := IntToStr(LValue.GlyphCell.Row);
  Result := NewNyxColumn(AID);
  Result.Configure.Gap(10).Padding(0).Done;
  Result.Node.SetProp(CMarkerKey, CMarker).SetProp(COwnerKey, AOwner.ID)
    .SetProp(CBaselineKey, NyxButtonEditorBaseline(AOwner, AProjection))
    .SetProp(CValueKey, LValue.ToData.ToJSON);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Choose a project image resource or an image URL. Glyphs stay decorative; the caption names the command.'));
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    LChoices := '';
    case LField of
      befContent: LChoices := CContent[nbcCaption] + #10 + CContent[nbcGlyphCaption] + #10 + CContent[nbcGlyphOnly];
      befDensity: LChoices := CDensity[nbdNormal] + #10 + CDensity[nbdCompact] + #10 + CDensity[nbdTouch];
      befAlignment: LChoices := CAlignment[nbaCenter] + #10 + CAlignment[nbaStart];
      befWeight: LChoices := CWeight[nbwInherit] + #10 + CWeight[nbwRegular] + #10 + CWeight[nbwSemibold];
      befTone: LChoices := CTone[ngtOriginal] + #10 + CTone[ngtForeground];
      befImage, befResource, befSize, befGap, befColumns, befRows, befColumn, befRow: ;
    end;

    if LChoices <> '' then
    begin
      LControl := NewNyxSelect(NyxButtonEditorFieldID(AID, LField));
      LControl.Configure.Items(LChoices).Done;
    end
    else
    begin
      LControl := NewNyxInput(NyxButtonEditorFieldID(AID, LField));

      if LField >= befSize then
      begin
        { These are unsubmitted authoring strings. Keep an incomplete/invalid
          number editable across refresh/recovery; Capture constructs Integers
          and the typed presentation validates every range before publication. }
        LControl.Configure.Placeholder('Whole number').Done;
      end;
    end;
    LControl.Configure.Text(CTitles[LField]).Value(LValues[LField]).Done;
    Result.Add(LControl);
  end;
  Result.Add(NewNyxButton(AID + '-apply').Configure.Text('Apply button style').Variant(nvPrimary).Done);
end;

function CaptureNyxButtonEditor(AButton, ARoot: TNyxNode;
  out AOwner, ABaseline: TNyxText; out AValue: TNyxButtonPresentation): Boolean;
var
  LEditor: TNyxNode;
  LOld: TNyxButtonPresentation;
  LImage, LResource: TNyxText;
  function Value(AField: TNyxButtonEditorField): TNyxText;
  begin
    Result := LEditor.Find(NyxButtonEditorFieldID(LEditor.ID, AField)).Prop('value');
  end;
  function Number(AField: TNyxButtonEditorField): Integer;
  begin

    if not TryStrToInt(Value(AField), Result) then
    begin
      raise ENyxModel.Create('Enter an Integer for ' + CTitles[AField]);
    end;
  end;
begin
  Result := False;
  AOwner := '';
  ABaseline := '';
  AValue := NyxButtonPresentation;

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LEditor := AButton.Parent;

  if not Complete(LEditor) or (AButton.ID <> LEditor.ID + '-apply') then
  begin
    Exit;
  end;
  LImage := Value(befImage);
  LResource := Value(befResource);

  if (LImage <> '') and (LResource <> '') then
  begin
    raise ENyxModel.Create('Choose an image source or a project resource, then clear the other field');
  end;
  LOld := TNyxButtonPresentation.FromData(TNyxDataValue.ParseJSON(LEditor.Prop(CValueKey)));

  if LImage <> '' then
  begin
    AValue := AValue.Glyph(TNyxImageSource.FromWire(LImage));
  end
  else if LResource <> '' then
  begin

    if (LOld.GlyphKind = nbgResource) and (LOld.ResourceImage.Reference.Name = LResource) then
    begin
      { Editing density must preserve an existing explicit localization choice. }
      AValue := AValue.Glyph(LOld.ResourceImage);
    end
    else
    begin
      AValue := AValue.Glyph(NyxResourceImage(NyxResourceRef(LResource)));
    end;
  end;
  AValue := AValue.Cell(NyxGlyphCell(Number(befColumns), Number(befRows),
    Number(befColumn), Number(befRow))).GlyphSize(Number(befSize))
    .GlyphGap(Number(befGap)).Content(TNyxButtonContent(Choice(Value(befContent), CContent)))
    .Density(TNyxButtonDensity(Choice(Value(befDensity), CDensity)))
    .Alignment(TNyxButtonContentAlignment(Choice(Value(befAlignment), CAlignment)))
    .FontWeight(TNyxButtonFontWeight(Choice(Value(befWeight), CWeight)))
    .GlyphTone(TNyxGlyphTone(Choice(Value(befTone), CTone)));
  AOwner := LEditor.Prop(COwnerKey);
  ABaseline := LEditor.Prop(CBaselineKey);
  Result := True;
end;

procedure TNyxButtonEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LField: TNyxButtonEditorField;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AID);

  if not Complete(LEditor) then
  begin
    Exit;
  end;
  FBaseline := LEditor.Prop(CBaselineKey);
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    FValues[LField] := LEditor.Find(NyxButtonEditorFieldID(AID, LField)).Prop('value');
  end;
end;

function TNyxButtonEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxButtonEditorField;
begin
  Result := False;

  if (ARoot = nil) or (FBaseline = '') then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AID);

  if not Complete(LEditor) or (LEditor.Prop(CBaselineKey) <> FBaseline) then
  begin
    Exit;
  end;
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    LEditor.Find(NyxButtonEditorFieldID(AID, LField)).Configure.Value(FValues[LField]).Done;
  end;
  Result := True;
end;

procedure TNyxButtonEditorDraft.Clear;
begin
  Self := Default(TNyxButtonEditorDraft);
end;

function TNyxButtonEditorDraft.GetDefined: Boolean;
begin
  Result := FBaseline <> '';
end;

function TNyxButtonEditorDraft.ToData: TNyxDataValue;
var
  LFields: array[TNyxButtonEditorField] of TNyxDataField;
  LField: TNyxButtonEditorField;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    LFields[LField] := NyxField(CKeys[LField], NyxData(FValues[LField]));
  end;
  Result := NyxObject([NyxField('baseline', NyxData(FBaseline)),
    NyxField('values', NyxObject(LFields))]);
end;

class function TNyxButtonEditorDraft.FromData(const AValue: TNyxDataValue): TNyxButtonEditorDraft;
var
  LField: TNyxButtonEditorField;
  LValues: TNyxDataValue;
begin
  Result := Default(TNyxButtonEditorDraft);

  if AValue.Kind = ndNull then
  begin
    Exit;
  end;

  if (AValue.Kind <> ndObject) or (AValue.Count <> 2) then
  begin
    raise ENyxModel.Create('Button draft requires its baseline and copied field values');
  end;
  Result.FBaseline := AValue.Field('baseline').AsText;
  LValues := AValue.Field('values');

  if (Result.FBaseline = '') or (LValues.Kind <> ndObject) or
    (LValues.Count <> Ord(High(TNyxButtonEditorField)) + 1) then
  begin
    raise ENyxModel.Create('Button draft requires its complete exact form');
  end;
  TNyxDataValue.ParseJSON(Result.FBaseline);
  for LField := Low(TNyxButtonEditorField) to High(TNyxButtonEditorField) do
  begin
    { Invalid unsubmitted choices are deliberately retained as text for repair. }
    Result.FValues[LField] := LValues.Field(CKeys[LField]).AsText;
  end;
end;

end.
