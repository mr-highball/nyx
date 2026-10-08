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
unit nyx.theme.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.colors, nyx.theme, nyx.design.tokens;

type
  { Closed fields/commands of the reusable theme form. Overrides are independent
    switches: unchecked roles inherit without fixing a host default into a design. }
  TNyxThemeEditorRole = (ntrBackground, ntrSurface, ntrText, ntrMuted, ntrBorder,
    ntrAccent, ntrAccentText, ntrRadius, ntrControlRadius, ntrFontSize);
  TNyxThemeEditorAction = (nteApply, nteReset, nteLight, nteDark);

  { Copied unsubmitted form. Values retain exact text until explicit Apply.
    No document, interface or widget is retained. Context checks include exact
    local presence and effective base; unrelated panel changes retain proposals,
    while changed tokens/host defaults or explicit project replacement retire them. }
  TNyxThemeEditorDraft = record
  private
    FEditorID: TNyxText;
    FContext: TNyxText;
    FValues: array[TNyxThemeEditorRole] of TNyxText;
    FOverrides: array[TNyxThemeEditorRole] of TNyxText;
    function GetDefined: Boolean;
  public
    { Absent parked forms retain input. Wrong context/field kinds retire it.
      Complete forms replace the copied draft atomically; parsing waits for Apply. }
    procedure Capture(const AEditorID: TNyxText; AShellRoot: TNyxNode);
    { All context and fields are checked before writing any value. Returns False
      for absent/incompatible forms. Sync the mounted view after restoration. }
    function Restore(AShellRoot: TNyxNode): Boolean;
    { Versioned preference boundary. Null is absent; malformed field counts or
      types refuse. This disposable presentation never enters design data. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxThemeEditorDraft; static;
    procedure Clear;
    property Defined: Boolean read GetDefined;
  end;

  { Immutable authoring proposal, with the exact declaration the user saw.
    Reset removes only this document's declaration. Controller admission owns
    paired history and must compare Baseline before publishing Tokens. }
  TNyxThemeEditorChange = record
    Baseline: TNyxText;
    Tokens: TNyxThemeTokens;
    Reset: Boolean;
  end;

function NyxThemeEditorFieldID(const AID: TNyxText; ARole: TNyxThemeEditorRole;
  AOverride: Boolean = False): TNyxText;
function NyxThemeEditorActionID(const AID: TNyxText;
  AAction: TNyxThemeEditorAction): TNyxText;
{ Exact declaration, including absent versus an explicitly empty object. }
function NyxThemeEditorBaseline(ADocument: TNyxDocument): TNyxText;
{ True only for a complete form with the current exact declaration baseline. }
function NyxThemeEditorMatches(AShellRoot: TNyxNode; const AID: TNyxText;
  ADocument: TNyxDocument): Boolean;
{ Builds an owned ordinary Nyx card with RGB controls and integer metrics.
  Document/base are borrowed only during construction. Presets are proposals;
  no accepted model or borrowed theme is changed by composing the form. }
function NewNyxThemeEditor(const AID: TNyxText; ADocument: TNyxDocument;
  ABase: TNyxTheme = nil): INyxCard;
{ Recognize only the exact mounted Apply/Reset button. Invalid selected fields,
  forged identities and incomplete forms raise ENyxModel/typed value errors.
  Reset remains available even with invalid unsubmitted input. }
function CaptureNyxThemeEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxThemeEditorChange): Boolean;
{ Recognize only the mounted Light/Dark proposal button. Returns an independent
  complete draft, ready for Restore, without accepting design or source/history. }
function PrepareNyxThemeEditorPreset(AButton, AShellRoot: TNyxNode;
  out ADraft: TNyxThemeEditorDraft): Boolean;

implementation

uses
  nyx.state, nyx.contract, nyx.layout.policy;

const
  CEditorKey = 'nyx.theme-editor';
  CBaselineKey = 'nyx.theme-editor.baseline';
  CEffectiveKey = 'nyx.theme-editor.effective';
  CNames: array[TNyxThemeEditorRole] of TNyxText = ('background', 'surface', 'text',
    'muted', 'border', 'accent', 'accentText', 'radius', 'controlRadius', 'fontSize');
  CLabels: array[TNyxThemeEditorRole] of TNyxText = ('Background', 'Surface',
    'Text', 'Secondary text', 'Border', 'Accent', 'Text on accent',
    'Surface radius', 'Control radius', 'Font size');
  CHints: array[TNyxThemeEditorRole] of TNyxText = (
    'Application background behind its surfaces.',
    'Panels, cards and input surfaces.',
    'Primary readable foreground text.',
    'Supporting captions and secondary text.',
    'Separators and control boundaries.',
    'Primary actions, selection and focus.',
    'Readable foreground on accent surfaces.',
    'Corner radius of cards and surfaces, in logical pixels.',
    'Corner radius of input controls, in logical pixels.',
    'Base application font size, in logical pixels.');

function NyxThemeEditorFieldID(const AID: TNyxText; ARole: TNyxThemeEditorRole;
  AOverride: Boolean): TNyxText;
begin
  Result := AID + TNyxText('-') + CNames[ARole];

  if AOverride then
  begin
    Result := Result + TNyxText('-override');
  end;
end;

function NyxThemeEditorActionID(const AID: TNyxText;
  AAction: TNyxThemeEditorAction): TNyxText;
const
  CNames: array[TNyxThemeEditorAction] of TNyxText = ('apply', 'reset', 'light', 'dark');
begin
  Result := AID + TNyxText('-') + CNames[AAction];
end;

function NyxThemeEditorBaseline(ADocument: TNyxDocument): TNyxText;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('A theme editor requires its document');
  end;
  Result := 'null';

  if ADocument.Extensions.Has(NyxExtension(NyxDesignTokensKey)) then
  begin
    Result := ADocument.Extensions.Value(NyxExtension(NyxDesignTokensKey)).ToJSON;
  end;
end;

function Context(AEditor: TNyxNode): TNyxText;
begin
  Result := NyxObject([
    NyxField('local', NyxData(AEditor.Prop(CBaselineKey))),
    NyxField('effective', NyxData(AEditor.Prop(CEffectiveKey)))]).ToJSON;
end;

function FieldsValid(AEditor: TNyxNode): Boolean;
var
  LRole: TNyxThemeEditorRole;
  LInput: TNyxNode;
  LCheck: TNyxNode;
  LKind: TNyxKind;
begin
  Result := False;

  if (AEditor = nil) or (AEditor.Kind <> NyxKindName(nkCard)) or
    (AEditor.Prop(CEditorKey) <> AEditor.ID) or
    (AEditor.Prop(CBaselineKey) = '') or (AEditor.Prop(CEffectiveKey) = '') then
  begin
    Exit;
  end;
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LInput := AEditor.Find(NyxThemeEditorFieldID(AEditor.ID, LRole));
    LCheck := AEditor.Find(NyxThemeEditorFieldID(AEditor.ID, LRole, True));
    LKind := nkColor;

    if LRole >= ntrRadius then
    begin
      LKind := nkSpin;
    end;

    if (LInput = nil) or (LInput.Kind <> NyxKindName(LKind)) or
      (LCheck = nil) or (LCheck.Kind <> NyxKindName(nkCheckbox)) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NyxThemeEditorMatches(AShellRoot: TNyxNode; const AID: TNyxText;
  ADocument: TNyxDocument): Boolean;
var
  LEditor: TNyxNode;
begin
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(AID);
  end;
  Result := (ADocument <> nil) and FieldsValid(LEditor) and
    (LEditor.Prop(CBaselineKey) = NyxThemeEditorBaseline(ADocument));
end;

function MountedAction(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode; out AAction: TNyxThemeEditorAction): Boolean;
var
  LID: TNyxText;
  LAction: TNyxThemeEditorAction;
begin
  Result := False;
  AEditor := nil;
  AAction := nteApply;

  if (AButton = nil) or (AButton.Prop(CEditorKey) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditorKey);

  if AShellRoot <> nil then
  begin
    AEditor := AShellRoot.Find(LID);
  end;

  if not FieldsValid(AEditor) or (AEditor.Find(AButton.ID) <> AButton) or
    (AButton.Kind <> NyxKindName(nkButton)) then
  begin
    raise ENyxModel.Create('Theme action requires its exact mounted form');
  end;
  for LAction := Low(TNyxThemeEditorAction) to High(TNyxThemeEditorAction) do
  begin

    if AButton.ID = NyxThemeEditorActionID(LID, LAction) then
    begin
      AAction := LAction;
      Exit(True);
    end;
  end;
  raise ENyxModel.Create('Unknown theme editor action');
end;

function NewNyxThemeEditor(const AID: TNyxText; ADocument: TNyxDocument;
  ABase: TNyxTheme): INyxCard;
var
  LEffective: TNyxTheme;
  LValues: TNyxThemeTokens;
  LDeclared: TNyxThemeTokens;
  LRole: TNyxThemeEditorRole;
  LRow: INyxColumn;
  LActions: INyxRow;
  LInput: INyxControl;
  LButton: INyxButton;
  LAction: TNyxThemeEditorAction;
  LOverridden: Boolean;
  LMinimum: Integer;
  LMaximum: Integer;
const
  CCaptions: array[TNyxThemeEditorAction] of TNyxText =
    ('Apply theme', 'Restore inherited theme', 'Light palette', 'Dark palette');
begin

  if AID = '' then
  begin
    raise ENyxModel.Create('Theme form requires an identity');
  end;
  NyxThemeEditorBaseline(ADocument);
  LDeclared := NyxDeclaredThemeTokens(ADocument);
  LEffective := NewNyxDocumentTheme(ADocument, ABase);
  try
    LValues := NyxThemeTokens
      .Background(TNyxRGBColor.FromText(LEffective.Background))
      .Surface(TNyxRGBColor.FromText(LEffective.Surface))
      .Text(TNyxRGBColor.FromText(LEffective.Text))
      .Muted(TNyxRGBColor.FromText(LEffective.Muted))
      .Border(TNyxRGBColor.FromText(LEffective.Border))
      .Accent(TNyxRGBColor.FromText(LEffective.Accent))
      .AccentText(TNyxRGBColor.FromText(LEffective.AccentText))
      .Radius(LEffective.Radius).ControlRadius(LEffective.ControlRadius)
      .FontSize(LEffective.FontSize);
  finally
    LEffective.Free;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Padding(12).Gap(10).Done;
  Result.Node.SetProp(CEditorKey, AID).SetProp(CBaselineKey,
    NyxThemeEditorBaseline(ADocument)).SetProp(CEffectiveKey, LValues.ToData.ToJSON);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).WithText('Application theme'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Override semantic roles, or inherit them. Palettes are proposals until Apply.'));
  LActions := NewNyxRow(AID + TNyxText('-presets'));
  LActions.Configure.Wrap(nfwWrap).Done;
  Result.Add(LActions);
  for LAction := nteLight to nteDark do
  begin
    LButton := NewNyxButton(NyxThemeEditorActionID(AID, LAction)).WithText(CCaptions[LAction]);
    LButton.Node.SetProp(CEditorKey, AID);
    LActions.Add(LButton);
  end;
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LRow := NewNyxColumn(AID + TNyxText('-role-') + CNames[LRole]);
    LRow.Configure.Gap(4).Done;
    Result.Add(LRow);

    if LRole < ntrRadius then
    begin
      LOverridden := LDeclared.Has(TNyxThemeColor(Ord(LRole)));
      LInput := NewNyxColor(NyxThemeEditorFieldID(AID, LRole))
        .WithColor(LValues.ColorValue(TNyxThemeColor(Ord(LRole))));
    end
    else
    begin
      LOverridden := LDeclared.Has(TNyxThemeMetric(Ord(LRole) - Ord(ntrRadius)));
      LMinimum := 0;
      LMaximum := 1000;

      if LRole = ntrFontSize then
      begin
        LMinimum := 1;
        LMaximum := 256;
      end;
      LInput := NewNyxSpin(NyxThemeEditorFieldID(AID, LRole));
      LInput.Configure.Minimum(LMinimum).Maximum(LMaximum).Done;
      LInput.Configure.Value(
        LValues.MetricValue(TNyxThemeMetric(Ord(LRole) - Ord(ntrRadius)))).Done;
      LInput.Contract.Value(NyxIntegerDomain.Range(LMinimum, LMaximum));
    end;
    LInput.Configure.Text(CLabels[LRole]).Hint(CHints[LRole]).Done;
    LRow.Add(LInput);
    LRow.Add(NewNyxCheckbox(NyxThemeEditorFieldID(AID, LRole, True))
      .Configure.Text('Override ' + CLabels[LRole]).Value(LOverridden).Done);
  end;
  for LAction := nteApply to nteReset do
  begin
    LButton := NewNyxButton(NyxThemeEditorActionID(AID, LAction)).WithText(CCaptions[LAction]);
    LButton.Node.SetProp(CEditorKey, AID);
    Result.Add(LButton);
  end;
end;

function TNyxThemeEditorDraft.GetDefined: Boolean;
begin
  Result := FEditorID <> '';
end;

procedure TNyxThemeEditorDraft.Clear;
begin
  Self := Default(TNyxThemeEditorDraft);
end;

procedure TNyxThemeEditorDraft.Capture(const AEditorID: TNyxText;
  AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;
  LRole: TNyxThemeEditorRole;
  LCandidate: TNyxThemeEditorDraft;
begin
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(AEditorID);
  end;

  if LEditor = nil then
  begin
    Exit;
  end;

  if not FieldsValid(LEditor) then
  begin
    Clear;
    Exit;
  end;
  LCandidate := Default(TNyxThemeEditorDraft);
  LCandidate.FEditorID := AEditorID;
  LCandidate.FContext := Context(LEditor);
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LCandidate.FValues[LRole] := LEditor.Find(
      NyxThemeEditorFieldID(AEditorID, LRole)).Prop('value');
    LCandidate.FOverrides[LRole] := LEditor.Find(
      NyxThemeEditorFieldID(AEditorID, LRole, True)).Prop('value');
  end;
  Self := LCandidate;
end;

function TNyxThemeEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LRole: TNyxThemeEditorRole;
begin
  Result := False;

  if not Defined or (AShellRoot = nil) then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(FEditorID);

  if LEditor = nil then
  begin
    Exit;
  end;

  if not FieldsValid(LEditor) or (Context(LEditor) <> FContext) then
  begin
    Clear;
    Exit;
  end;
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LEditor.Find(NyxThemeEditorFieldID(FEditorID, LRole)).SetProp('value', FValues[LRole]);
    LEditor.Find(NyxThemeEditorFieldID(FEditorID, LRole, True))
      .SetProp('value', FOverrides[LRole]);
  end;
  Result := True;
end;

function CaptureNyxThemeEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxThemeEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxThemeEditorAction;
  LRole: TNyxThemeEditorRole;
  LChecked: TNyxText;
  LText: TNyxText;
  LMetric: Integer;
begin
  AChange := Default(TNyxThemeEditorChange);
  Result := MountedAction(AButton, AShellRoot, LEditor, LAction) and
    (LAction in [nteApply, nteReset]);

  if not Result then
  begin
    Exit;
  end;
  AChange.Baseline := LEditor.Prop(CBaselineKey);
  AChange.Reset := LAction = nteReset;
  AChange.Tokens := NyxThemeTokens;

  if AChange.Reset then
  begin
    Exit;
  end;
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LChecked := LEditor.Find(NyxThemeEditorFieldID(LEditor.ID, LRole, True)).Prop('value');

    if (LChecked <> 'true') and (LChecked <> 'false') then
    begin
      raise ENyxModel.Create('Theme override requires a Boolean choice');
    end;

    if LChecked = 'false' then
    begin
      Continue;
    end;
    LText := LEditor.Find(NyxThemeEditorFieldID(LEditor.ID, LRole)).Prop('value');

    if LRole < ntrRadius then
    begin
      AChange.Tokens := AChange.Tokens.Color(TNyxThemeColor(Ord(LRole)),
        TNyxRGBColor.FromText(LText));
    end
    else
    begin

      if not TryNyxStateInteger(LText, LMetric) then
      begin
        raise ENyxModel.Create('Theme metrics require whole logical pixels');
      end;
      AChange.Tokens := AChange.Tokens.Metric(
        TNyxThemeMetric(Ord(LRole) - Ord(ntrRadius)), LMetric);
    end;
  end;
end;

function TNyxThemeEditorDraft.ToData: TNyxDataValue;
var
  LRole: TNyxThemeEditorRole;
  LValues: array of TNyxDataValue;
  LOverrides: array of TNyxDataValue;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  SetLength(LValues, Ord(High(TNyxThemeEditorRole)) + 1);
  SetLength(LOverrides, Length(LValues));
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    LValues[Ord(LRole)] := NyxData(FValues[LRole]);
    LOverrides[Ord(LRole)] := NyxData(FOverrides[LRole]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('editor', NyxData(FEditorID)), NyxField('context', NyxData(FContext)),
    NyxField('values', NyxArray(LValues)), NyxField('overrides', NyxArray(LOverrides))]);
end;

class function TNyxThemeEditorDraft.FromData(const AData: TNyxDataValue): TNyxThemeEditorDraft;
var
  LRole: TNyxThemeEditorRole;
  LValues: TNyxDataValue;
  LOverrides: TNyxDataValue;
begin
  Result := Default(TNyxThemeEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported theme form preference');
  end;
  Result.FEditorID := AData.Field('editor').AsText;
  Result.FContext := AData.Field('context').AsText;
  LValues := AData.Field('values');
  LOverrides := AData.Field('overrides');

  if (Result.FEditorID = '') or (Result.FContext = '') or
    (LValues.Kind <> ndArray) or (LValues.Count <> Ord(High(TNyxThemeEditorRole)) + 1) or
    (LOverrides.Kind <> ndArray) or (LOverrides.Count <> LValues.Count) then
  begin
    raise ENyxModel.Create('Theme form preference requires its complete context and fields');
  end;
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    Result.FValues[LRole] := LValues.Item(Ord(LRole)).AsText;
    Result.FOverrides[LRole] := LOverrides.Item(Ord(LRole)).AsText;
  end;
end;

function PrepareNyxThemeEditorPreset(AButton, AShellRoot: TNyxNode;
  out ADraft: TNyxThemeEditorDraft): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxThemeEditorAction;
  LRole: TNyxThemeEditorRole;
  LValues: TNyxDataValue;
  LPreset: TNyxThemePreset;
begin
  ADraft := Default(TNyxThemeEditorDraft);
  Result := MountedAction(AButton, AShellRoot, LEditor, LAction) and
    (LAction in [nteLight, nteDark]);

  if not Result then
  begin
    Exit;
  end;
  LPreset := ntpLight;

  if LAction = nteDark then
  begin
    LPreset := ntpDark;
  end;
  LValues := NyxThemePreset(LPreset).ToData;
  ADraft.Capture(LEditor.ID, AShellRoot);
  for LRole := Low(TNyxThemeEditorRole) to High(TNyxThemeEditorRole) do
  begin
    ADraft.FOverrides[LRole] := 'true';

    if LRole < ntrRadius then
    begin
      ADraft.FValues[LRole] := LValues.Field(CNames[LRole]).AsText;
    end
    else
    begin
      ADraft.FValues[LRole] := TNyxText(IntToStr(LValues.Field(CNames[LRole]).AsInteger));
    end;
  end;
end;

end.
