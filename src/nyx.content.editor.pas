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
unit nyx.content.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.content, nyx.responsive, nyx.presentations;

type
  { Closed fields of the reusable editor. IDs are stable composition identities,
    not executable property names. Sizes are logical available-content pixels;
    minimum is inclusive, maximum exclusive, and zero maximum means unbounded. }
  TNyxContentEditorField = (ncfScope, ncfPlatform, ncfRecipe, ncfPresentation,
    ncfWidthMinimum, ncfWidthMaximum, ncfHeightMinimum, ncfHeightMaximum,
    ncfOrientation, ncfApply);

  { Closed local row actions. Edit changes only the disposable form; Remove
    continues through the ordinary paired design admission path. }
  TNyxContentRuleAction = (ncraEdit, ncraRemove);

  { Copied proposal for one recipe form. Field values are not parsed until Apply.
    Context includes the registry, compatible default and exact available choices,
    so identical IDs with changed dependencies cannot receive stale input.
    This record owns no control, interface, document or renderer and never enters
    design/source history. Ordinary record copies remain independent. }
  TNyxContentEditorDraft = record
  private
    FEditorID: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FDefault: TNyxText;
    FRecipes: TNyxText;
    FPresentations: TNyxText;
    FEditing: Boolean;
    FEditingIndex: Integer;
    FValues: array[ncfScope..ncfOrientation] of TNyxText;
    function GetDefined: Boolean;
  public
    { An absent form keeps parked input. Missing context or wrong field kinds
      retire it; a complete form replaces it atomically without author admission. }
    procedure Capture(const AEditorID: TNyxText; AShellRoot: TNyxNode);
    { Validate every context value/field before any write. Absent forms keep
      parked input; a different owner/default/registry/choice list or field shape
      retires it. Call before rendering, or Sync an already mounted form after
      successful Restore. Opaque physical spin-edit buffers are not captured. }
    function Restore(AShellRoot: TNyxNode): Boolean;
    { Fresh owning document check for a local edit-row action. Nil/undefined
      inputs return False; no document or supplied registry is changed. }
    function Matches(const AOwner: TNyxControlRef; const AContent: INyxContent;
      const ADefault: TNyxComponentRef): Boolean;
    { Explicit project replacement retires even identical owner/baseline text. }
    procedure Clear;
    property Defined: Boolean read GetDefined;
  end;

  { Capture owns an independent registry and exact mounted baseline. The caller
    must compare that baseline with its current owner before admitting the edit.
    No document, rendered control or recipe definition survives capture. }
  TNyxContentEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
  end;

function NyxContentEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxContentEditorField): TNyxText;
{ Exact row identity; indices are 0..63, matching the portable registry budget. }
function NyxContentEditorRuleID(const AEditorID: TNyxText; AIndex: Integer;
  AAction: TNyxContentRuleAction): TNyxText;
{ Closed selector vocabulary at the mounted form's display/input boundary. }
function NyxContentEditorScopeName(AScope: TNyxContentScope): TNyxText;
function NyxContentEditorPlatformName(APlatform: TNyxPlatform): TNyxText;
function NyxContentEditorOrientationName(AOrientation: TNyxViewportOrientation): TNyxText;
{ Public Nyx compound: specialized card/select/spin/button/label interfaces.
  All inputs are borrowed only during composition, then copied into immutable
  metadata. The returned card and its parent own descendants normally. Application
  names retain exact Unicode; separate fields prevent caption/name collisions.
  Empty choices remain visible but cannot be applied. Existing rules show their
  exact scope and offer editing/removal. Editing replaces the original row in
  evaluation order; a scope collision with a different row refuses. }
function NewNyxContentEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const AContent: INyxContent; const ADefault: TNyxComponentRef;
  const ARecipes: array of TNyxComponentRef;
  const APresentations: array of TNyxPresentationRef): INyxCard;
{ Recognizes only a mounted editor's own apply/remove buttons. Unrelated controls
  return False. Missing fields, stale/forged buttons, invalid choices and incomplete
  intervals raise without changing the editor or accepted model. Closed choice
  failures use ENyxContent; invalid numeric intervals use EArgumentException.
  AContent returns an independent complete candidate; document admission remains
  responsible for every recipe dependency and inactive branch. }
function CaptureNyxContentEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxContentEditorChange; out AContent: INyxContent): Boolean;
{ Recognizes an exact mounted Edit row and returns a complete prefilling draft.
  Unrelated/Apply/Remove controls return False. Forged/stale rows, unavailable
  references and incomplete fields raise ENyxContent. It modifies neither the
  mounted form nor accepted content; the owning controller rechecks Matches.
  The output remains undefined on refusal. Apply still owns model publication. }
function CaptureNyxContentEditorRule(AButton, AShellRoot: TNyxNode;
  out ADraft: TNyxContentEditorDraft): Boolean;

implementation

const
  CEditorKey = 'nyx.content-editor';
  COwnerKey = 'nyx.content-editor.owner';
  CBaselineKey = 'nyx.content-editor.baseline';
  CRecipesKey = 'nyx.content-editor.recipes';
  CPresentationsKey = 'nyx.content-editor.presentations';
  CRemoveKey = 'nyx.content-editor.remove';
  CEditKey = 'nyx.content-editor.edit';
  CDefaultKey = 'nyx.content-editor.default';
  CVersionKey = 'nyx.content-editor.version';
  CEditingKey = 'nyx.content-editor.editing';
  CFields: array[TNyxContentEditorField] of TNyxText =
    ('scope', 'platform', 'recipe', 'presentation', 'width-minimum', 'width-maximum',
      'height-minimum', 'height-maximum', 'orientation', 'apply');
  CScopes: array[TNyxContentScope] of TNyxText =
    ('Default', 'Available size', 'Named presentation');
  CPlatforms: array[TNyxPlatform] of TNyxText = ('All targets', 'Browser', 'Native LCL');
  COrientations: array[TNyxViewportOrientation] of TNyxText =
    ('Any orientation', 'Portrait', 'Landscape', 'Square');
  CInputKinds: array[ncfScope..ncfOrientation] of TNyxKind =
    (nkSelect, nkSelect, nkSelect, nkSelect, nkSpin, nkSpin, nkSpin, nkSpin, nkSelect);

function NyxContentEditorScopeName(AScope: TNyxContentScope): TNyxText;
begin
  Result := CScopes[AScope];
end;

function NyxContentEditorPlatformName(APlatform: TNyxPlatform): TNyxText;
begin
  Result := CPlatforms[APlatform];
end;

function NyxContentEditorOrientationName(AOrientation: TNyxViewportOrientation): TNyxText;
begin
  Result := COrientations[AOrientation];
end;

function NyxContentEditorRuleID(const AEditorID: TNyxText; AIndex: Integer;
  AAction: TNyxContentRuleAction): TNyxText;
const
  CActions: array[TNyxContentRuleAction] of TNyxText = ('edit', 'remove');
begin

  if (AEditorID = '') or (AIndex < 0) or (AIndex >= NyxMaximumContentRules) then
  begin
    raise ENyxContent.Create('Content row requires an editor identity and registered index');
  end;
  Result := AEditorID + TNyxText('-rule-') + TNyxText(IntToStr(AIndex)) +
    TNyxText('-') + CActions[AAction];
end;

function TNyxContentEditorDraft.GetDefined: Boolean;
begin
  Result := FEditorID <> '';
end;

procedure TNyxContentEditorDraft.Clear;
begin
  Self := Default(TNyxContentEditorDraft);
end;

procedure TNyxContentEditorDraft.Capture(const AEditorID: TNyxText;
  AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
  LField: TNyxContentEditorField;
  LCandidate: TNyxContentEditorDraft;
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

  if (AEditorID = '') or (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.Prop(CVersionKey) <> '2') or
    (LEditor.Prop(COwnerKey) = '') or (LEditor.Prop(CBaselineKey) = '') or
    (LEditor.Prop(CRecipesKey) = '') or (LEditor.Prop(CPresentationsKey) = '') then
  begin
    Clear;
    Exit;
  end;
  LCandidate := Default(TNyxContentEditorDraft);
  LCandidate.FEditorID := AEditorID;
  LCandidate.FOwner := LEditor.Prop(COwnerKey);
  LCandidate.FBaseline := LEditor.Prop(CBaselineKey);
  LCandidate.FDefault := LEditor.Prop(CDefaultKey);
  LCandidate.FRecipes := LEditor.Prop(CRecipesKey);
  LCandidate.FPresentations := LEditor.Prop(CPresentationsKey);

  if LEditor.Prop(CEditingKey) <> '' then
  begin

    if not TryStrToInt(LEditor.Prop(CEditingKey), LCandidate.FEditingIndex) or
      (LCandidate.FEditingIndex < 0) or (LCandidate.FEditingIndex >= NyxMaximumContentRules) then
    begin
      Clear;
      Exit;
    end;
    LCandidate.FEditing := True;
  end;
  for LField := ncfScope to ncfOrientation do
  begin
    LInput := LEditor.Find(NyxContentEditorFieldID(AEditorID, LField));

    if (LInput = nil) or (LInput.Kind <> NyxKindName(CInputKinds[LField])) then
    begin
      Clear;
      Exit;
    end;
    LCandidate.FValues[LField] := LInput.Prop('value');
  end;
  Self := LCandidate;
end;

function TNyxContentEditorDraft.Matches(const AOwner: TNyxControlRef;
  const AContent: INyxContent; const ADefault: TNyxComponentRef): Boolean;
begin
  Result := Defined and (AContent <> nil);

  if Result then
  begin
    Result := (FOwner = AOwner.ID) and (FDefault = ADefault.Name) and
      (FBaseline = AContent.ToData.ToJSON);
  end;
end;

function TNyxContentEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
  LField: TNyxContentEditorField;
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

  if (LEditor.Kind <> NyxKindName(nkCard)) or (LEditor.Prop(COwnerKey) <> FOwner) or
    (LEditor.Prop(CVersionKey) <> '2') or
    (LEditor.Prop(CBaselineKey) <> FBaseline) or (LEditor.Prop(CDefaultKey) <> FDefault) or
    (LEditor.Prop(CRecipesKey) <> FRecipes) or
    (LEditor.Prop(CPresentationsKey) <> FPresentations) then
  begin
    Clear;
    Exit;
  end;
  for LField := ncfScope to ncfOrientation do
  begin
    LInput := LEditor.Find(NyxContentEditorFieldID(FEditorID, LField));

    if (LInput = nil) or (LInput.Kind <> NyxKindName(CInputKinds[LField])) then
    begin
      Clear;
      Exit;
    end;
  end;
  for LField := ncfScope to ncfOrientation do
  begin
    { This is the copied draft boundary, not fluent authoring/admission. Numeric
      fields may contain incomplete or invalid proposals until Apply. Preserve
      exact text after all field kinds/context match, rather than invoking the
      typed value overload (which correctly refuses a String for Integer). }
    LEditor.Find(NyxContentEditorFieldID(FEditorID, LField))
      .SetProp(NyxAttributeName(atValue), FValues[LField]);
  end;

  if FEditing then
  begin
    LEditor.SetProp(CEditingKey, TNyxText(IntToStr(FEditingIndex)));
  end
  else
  begin
    LEditor.SetProp(CEditingKey, '');
  end;
  Result := True;
end;

function NyxContentEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxContentEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

function RuleCaption(const ARule: TNyxContentRule): TNyxText;
begin
  Result := CScopes[ARule.Scope];

  if ARule.Scope = ncsViewport then
  begin
    Result := ARule.Viewport.Caption;
  end
  else if ARule.Scope = ncsPresentation then
  begin
    Result := TNyxText('Presentation / ') + ARule.Presentation.Name;
  end;
  Result := Result + TNyxText(' · ') + CPlatforms[ARule.Platform] + TNyxText(' → ') +
    ARule.Component.Name;
end;

function NewNyxContentEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const AContent: INyxContent; const ADefault: TNyxComponentRef;
  const ARecipes: array of TNyxComponentRef;
  const APresentations: array of TNyxPresentationRef): INyxCard;
var
  LContent: INyxContent;
  LRecipes: array of TNyxDataValue;
  LPresentations: array of TNyxDataValue;
  LItems: TNyxStrings;
  LNames: TNyxText;
  LFirst: TNyxText;
  LIndex: Integer;
  LRow: INyxColumn;
  LButton: INyxButton;
begin

  if (AID = '') or (AOwner.ID = '') then
  begin
    raise ENyxContent.Create('Content editor requires exact editor and control identities');
  end;
  LContent := NewNyxContent;

  if AContent <> nil then
  begin
    LContent := AContent.Clone.Done;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.Node.SetProp(COwnerKey, AOwner.ID).SetProp(CBaselineKey, LContent.ToData.ToJSON);
  Result.Node.SetProp(CDefaultKey, ADefault.Name).SetProp(CVersionKey, '2');
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).Configure.Text('Content recipes').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure.Text(
    'Use a different reusable component for a size, presentation or target. Each choice replaces the whole recipe.').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-default')).Configure.Text(
    TNyxText('Current default / ') + ADefault.Name).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfScope)).Configure.Text('When')
    .Items(CScopes[ncsDefault] + #10 + CScopes[ncsViewport] + #10 + CScopes[ncsPresentation])
    .Value(CScopes[ncsDefault]).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfPlatform)).Configure.Text('Target')
    .Items(CPlatforms[npfAny] + #10 + CPlatforms[npfBrowser] + #10 + CPlatforms[npfNativeLCL])
    .Value(CPlatforms[npfAny]).Done);
  LItems := TNyxStrings.Create;
  try
    SetLength(LRecipes, Length(ARecipes));
    for LIndex := 0 to High(ARecipes) do
    begin
      LRecipes[LIndex] := NyxData(ARecipes[LIndex].Name);
      LItems.Add(ARecipes[LIndex].Name);
    end;
    LNames := LItems.Join(#10);
    LFirst := '';

    if Length(ARecipes) > 0 then
    begin
      LFirst := ARecipes[0].Name;
    end;
    for LIndex := 0 to High(ARecipes) do
    begin

      if ARecipes[LIndex].Name = ADefault.Name then
      begin
        LFirst := ADefault.Name;
        Break;
      end;
    end;
    Result.Node.SetProp(CRecipesKey, NyxArray(LRecipes).ToJSON);
    Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfRecipe)).Configure.Text('Reusable recipe')
      .Items(LNames).Value(LFirst).Enabled(Length(ARecipes) > 0).Done);
    LItems.Clear;
    SetLength(LPresentations, Length(APresentations));
    for LIndex := 0 to High(APresentations) do
    begin
      LPresentations[LIndex] := NyxData(APresentations[LIndex].Name);
      LItems.Add(APresentations[LIndex].Name);
    end;
    LNames := LItems.Join(#10);
    LFirst := '';

    if Length(APresentations) > 0 then
    begin
      LFirst := APresentations[0].Name;
    end;
    Result.Node.SetProp(CPresentationsKey, NyxArray(LPresentations).ToJSON);
    Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfPresentation)).Configure.Text('Named presentation')
      .Items(LNames).Value(LFirst).Enabled(Length(APresentations) > 0).Done);
  finally
    LItems.Free;
  end;
  Result.Add(NewNyxLabel(AID + TNyxText('-size-help')).Configure.Text(
    'Size fields apply to Available size. Minimums include the boundary; upper bounds exclude it. Zero upper bound means no limit.').Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfWidthMinimum)).Configure
    .Text('Minimum width').Minimum(0).Maximum(High(Integer)).Value(0).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfWidthMaximum)).Configure
    .Text('Below width').Minimum(0).Maximum(High(Integer)).Value(640).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfHeightMinimum)).Configure
    .Text('Minimum height').Minimum(0).Maximum(High(Integer)).Value(0).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfHeightMaximum)).Configure
    .Text('Below height').Minimum(0).Maximum(High(Integer)).Value(0).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfOrientation)).Configure.Text('Available orientation')
    .Items(COrientations[nvoAny] + #10 + COrientations[nvoPortrait] + #10 +
      COrientations[nvoLandscape] + #10 + COrientations[nvoSquare])
    .Value(COrientations[nvoAny]).Done);
  LButton := NewNyxButton(NyxContentEditorFieldID(AID, ncfApply));
  LButton.Configure.Text('Use recipe in this scope').Enabled(Length(ARecipes) > 0).Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
  for LIndex := 0 to LContent.Count - 1 do
  begin
    LRow := NewNyxColumn(AID + TNyxText('-rule-') + TNyxText(IntToStr(LIndex)));
    LRow.Configure.Gap(4).Done;
    LRow.Add(NewNyxLabel(LRow.ID + TNyxText('-caption')).Configure
      .Text(RuleCaption(LContent.Rule(LIndex))).Done);
    LButton := NewNyxButton(NyxContentEditorRuleID(AID, LIndex, ncraEdit));
    LButton.Configure.Text('Edit this choice').Done;
    LButton.Node.SetProp(CEditorKey, AID).SetProp(CEditKey, TNyxText(IntToStr(LIndex)));
    LRow.Add(LButton);
    LButton := NewNyxButton(NyxContentEditorRuleID(AID, LIndex, ncraRemove));
    LButton.Configure.Text('Remove this choice').Done;
    LButton.Node.SetProp(CEditorKey, AID).SetProp(CRemoveKey, TNyxText(IntToStr(LIndex)));
    LRow.Add(LButton);
    Result.Add(LRow);
  end;
end;

function CaptureNyxContentEditorRule(AButton, AShellRoot: TNyxNode;
  out ADraft: TNyxContentEditorDraft): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LIndex: Integer;
  LContent: INyxContent;
  LRule: TNyxContentRule;
  LCandidate: TNyxContentEditorDraft;

  procedure RequireChoice(const AKey, AValue: TNyxText);
  var
    LChoices: TNyxDataValue;
    LChoice: Integer;
  begin
    LChoices := TNyxDataValue.ParseJSON(LEditor.Prop(AKey));

    if LChoices.Kind <> ndArray then
    begin
      raise ENyxContent.Create('Recipe editor choices are no longer available');
    end;
    for LChoice := 0 to LChoices.Count - 1 do
    begin

      if (LChoices.Item(LChoice).Kind = ndText) and
        (LChoices.Item(LChoice).AsText = AValue) then
      begin
        Exit;
      end;
    end;
    raise ENyxContent.Create('Select this recipe again after its choices are refreshed');
  end;
begin
  ADraft := Default(TNyxContentEditorDraft);
  Result := (AButton <> nil) and (AButton.Prop(CEditKey) <> '');

  if not Result then
  begin
    Exit;
  end;
  LEditor := nil;
  LEditorID := AButton.Prop(CEditorKey);

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(LEditorID);
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) or
    not TryStrToInt(AButton.Prop(CEditKey), LIndex) then
  begin
    raise ENyxContent.Create('Select the current recipe row before editing');
  end;

  if AButton.ID <> NyxContentEditorRuleID(LEditorID, LIndex, ncraEdit) then
  begin
    raise ENyxContent.Create('Recipe editing requires its exact mounted row');
  end;
  LCandidate := Default(TNyxContentEditorDraft);
  LCandidate.Capture(LEditorID, AShellRoot);

  if not LCandidate.Defined then
  begin
    raise ENyxContent.Create('Recipe editor fields are no longer complete');
  end;
  LContent := NyxContentFromData(TNyxDataValue.ParseJSON(LCandidate.FBaseline));
  LRule := LContent.Rule(LIndex);
  LCandidate.FEditing := True;
  LCandidate.FEditingIndex := LIndex;
  RequireChoice(CRecipesKey, LRule.Component.Name);
  LCandidate.FValues[ncfScope] := NyxContentEditorScopeName(LRule.Scope);
  LCandidate.FValues[ncfPlatform] := NyxContentEditorPlatformName(LRule.Platform);
  LCandidate.FValues[ncfRecipe] := LRule.Component.Name;
  LCandidate.FValues[ncfWidthMinimum] := '0';
  LCandidate.FValues[ncfWidthMaximum] := '0';
  LCandidate.FValues[ncfHeightMinimum] := '0';
  LCandidate.FValues[ncfHeightMaximum] := '0';
  LCandidate.FValues[ncfOrientation] := NyxContentEditorOrientationName(nvoAny);

  if LRule.Scope = ncsViewport then
  begin
    LCandidate.FValues[ncfWidthMinimum] := TNyxText(IntToStr(LRule.Viewport.WidthMinimum));
    LCandidate.FValues[ncfWidthMaximum] := TNyxText(IntToStr(LRule.Viewport.WidthMaximum));
    LCandidate.FValues[ncfHeightMinimum] := TNyxText(IntToStr(LRule.Viewport.HeightMinimum));
    LCandidate.FValues[ncfHeightMaximum] := TNyxText(IntToStr(LRule.Viewport.HeightMaximum));
    LCandidate.FValues[ncfOrientation] :=
      NyxContentEditorOrientationName(LRule.Viewport.OrientationValue);
  end
  else if LRule.Scope = ncsPresentation then
  begin
    RequireChoice(CPresentationsKey, LRule.Presentation.Name);
    LCandidate.FValues[ncfPresentation] := LRule.Presentation.Name;
  end;
  ADraft := LCandidate;
end;

function CaptureNyxContentEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxContentEditorChange; out AContent: INyxContent): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LScope: TNyxContentScope;
  LPlatform: TNyxPlatform;
  LOrientation: TNyxViewportOrientation;
  LCondition: TNyxViewportCondition;
  LIndex: Integer;
  LRule: TNyxContentRule;
  LTarget: INyxContent;
  LMinimum: Integer;
  LMaximum: Integer;
  LEditingIndex: Integer;
  LProposed: INyxContent;
  LRevised: INyxContent;
  LProposedRule: TNyxContentRule;

  procedure AppendRule(const ARegistry: INyxContent; const ARule: TNyxContentRule);
  var
    LScopeTarget: INyxContent;
  begin
    LScopeTarget := ARegistry.Done;
    case ARule.Scope of
      ncsDefault:
        begin
          LScopeTarget := ARegistry.Done;
        end;
      ncsViewport:
        begin
          LScopeTarget := LScopeTarget.WhenViewport(ARule.Viewport);
        end;
      ncsPresentation:
        begin
          LScopeTarget := LScopeTarget.WhenPresentation(ARule.Presentation);
        end;
    end;
    LScopeTarget.ForPlatform(ARule.Platform).Use(ARule.Component);
  end;

  function Value(AField: TNyxContentEditorField): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxContentEditorFieldID(LEditorID, AField));

    if LField = nil then
    begin
      raise ENyxContent.Create('Content editor field is no longer mounted');
    end;
    Result := LField.Prop('value');
  end;

  function Number(AField: TNyxContentEditorField): Integer;
  begin

    if not TryStrToInt(Value(AField), Result) or (Result < 0) then
    begin
      raise ENyxContent.Create('Content sizes require complete nonnegative 32-bit integers');
    end;
  end;

  function Choice(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
  var
    LChoice: Integer;
  begin
    for LChoice := 0 to High(AChoices) do
    begin

      if AValue = AChoices[LChoice] then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxContent.Create('Choose an available content scope, target or orientation');
  end;

  function Reference(const AKey: TNyxText; AField: TNyxContentEditorField): TNyxText;
  var
    LValues: TNyxDataValue;
    LChoice: Integer;
  begin
    Result := Value(AField);
    LValues := TNyxDataValue.ParseJSON(LEditor.Prop(AKey));
    for LChoice := 0 to LValues.Count - 1 do
    begin

      if Result = LValues.Item(LChoice).AsText then
      begin
        Exit;
      end;
    end;
    raise ENyxContent.Create('Choose an available reusable recipe or named presentation');
  end;
begin
  AChange := Default(TNyxContentEditorChange);
  AContent := nil;
  Result := (AButton <> nil) and (AButton.Prop(CEditorKey) <> '');

  if Result and (AButton.Prop(CEditKey) <> '') then
  begin
    { Local prefill is not a document mutation/source preparation request. }
    Result := False;
  end;

  if not Result then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CEditorKey);
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(LEditorID);
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxContent.Create('Select the current content editor before applying a choice');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwnerKey));
  AChange.Baseline := LEditor.Prop(CBaselineKey);
  AContent := NyxContentFromData(TNyxDataValue.ParseJSON(AChange.Baseline));

  if AButton.Prop(CRemoveKey) <> '' then
  begin

    if not TryStrToInt(AButton.Prop(CRemoveKey), LIndex) then
    begin
      raise ENyxContent.Create('Recipe removal requires its exact registered scope');
    end;
    LRule := AContent.Rule(LIndex);
    LTarget := AContent;
    case LRule.Scope of
      ncsDefault: LTarget := AContent;
      ncsViewport: LTarget := LTarget.WhenViewport(LRule.Viewport);
      ncsPresentation: LTarget := LTarget.WhenPresentation(LRule.Presentation);
    end;
    LTarget.ForPlatform(LRule.Platform).Clear;
    AContent := AContent.Done;
    Exit;
  end;

  if AButton.ID <> NyxContentEditorFieldID(LEditorID, ncfApply) then
  begin
    raise ENyxContent.Create('Unknown content editor command');
  end;
  LScope := TNyxContentScope(Choice(Value(ncfScope), CScopes));
  LPlatform := TNyxPlatform(Choice(Value(ncfPlatform), CPlatforms));
  LProposed := NewNyxContent;
  LTarget := LProposed;
  case LScope of
    ncsDefault: LTarget := LProposed;
    ncsViewport:
      begin
        LOrientation := TNyxViewportOrientation(Choice(Value(ncfOrientation), COrientations));
        LCondition := TNyxViewportCondition.Any.Orientation(LOrientation);
        LMinimum := Number(ncfWidthMinimum);
        LMaximum := Number(ncfWidthMaximum);

        if LMaximum = 0 then
        begin
          LCondition := LCondition.WidthAtLeast(LMinimum);
        end
        else
        begin
          LCondition := LCondition.WidthBetween(LMinimum, LMaximum);
        end;
        LMinimum := Number(ncfHeightMinimum);
        LMaximum := Number(ncfHeightMaximum);

        if LMaximum = 0 then
        begin
          LCondition := LCondition.HeightAtLeast(LMinimum);
        end
        else
        begin
          LCondition := LCondition.HeightBetween(LMinimum, LMaximum);
        end;

        if LCondition.IsAny then
        begin
          raise ENyxContent.Create('Choose Default for an unrestricted size');
        end;
        LTarget := LTarget.WhenViewport(LCondition);
      end;
    ncsPresentation:
      LTarget := LTarget.WhenPresentation(NyxPresentation(Reference(CPresentationsKey, ncfPresentation)));
  end;
  LTarget.ForPlatform(LPlatform).Use(NyxComponent(Reference(CRecipesKey, ncfRecipe)));
  LProposedRule := LProposed.Rule(0);

  if LEditor.Prop(CEditingKey) = '' then
  begin
    AppendRule(AContent, LProposedRule);
  end
  else
  begin

    if not TryStrToInt(LEditor.Prop(CEditingKey), LEditingIndex) or
      (LEditingIndex < 0) or (LEditingIndex >= AContent.Count) then
    begin
      raise ENyxContent.Create('Select the registered recipe choice again before applying');
    end;
    { Changing a scope replaces its original row, preserving evaluation order.
      Colliding with another existing row refuses instead of deleting/merging
      that independently authored choice. Nothing accepted has changed yet. }
    LRevised := NewNyxContent;
    for LIndex := 0 to AContent.Count - 1 do
    begin
      LRule := AContent.Rule(LIndex);

      if (LIndex <> LEditingIndex) and LRule.SameScope(LProposedRule) then
      begin
        raise ENyxContent.Create('Another choice uses this scope; edit that choice or choose a different scope');
      end;

      if LIndex = LEditingIndex then
      begin
        AppendRule(LRevised, LProposedRule);
      end
      else
      begin
        AppendRule(LRevised, LRule);
      end;
    end;
    AContent := LRevised;
  end;
  AContent := AContent.Done;
end;

end.
